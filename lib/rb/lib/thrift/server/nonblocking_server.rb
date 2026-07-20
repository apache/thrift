# frozen_string_literal: true
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

require 'logger'
require 'thread'

module Thrift
  # this class expects to always use a FramedTransport for reading messages
  class NonblockingServer < BaseServer
    def initialize(processor, server_transport, transport_factory = nil, protocol_factory = nil, num = 20, logger = nil)
      super(processor, server_transport, transport_factory, protocol_factory)
      @num_threads = num
      if logger.nil?
        @logger = Logger.new(STDERR)
        @logger.level = Logger::WARN
      else
        @logger = logger
      end
      @shutdown_semaphore = Mutex.new
      @transport_semaphore = Mutex.new
    end

    def serve
      @logger.info "Starting #{self}"
      @server_transport.listen
      @io_manager = start_io_manager

      begin
        loop do
          break if @server_transport.closed?
          begin
            rd, = select([@server_transport], nil, nil, 0.1)
          rescue Errno::EBADF => e
            # In Ruby 1.9, calling @server_transport.close in shutdown paths causes the select() to raise an
            # Errno::EBADF. If this happens, ignore it and retry the loop.
            break
          end
          next if rd.nil?
          socket = @server_transport.accept
          @logger.debug "Accepted socket: #{socket.inspect}"
          @io_manager.add_connection socket
        end
      rescue IOError => e
      end
      # we must be shutting down
      @logger.info "#{self} is shutting down, goodbye"
    ensure
      @transport_semaphore.synchronize do
        @server_transport.close
      end
      @io_manager.ensure_closed unless @io_manager.nil?
    end

    def shutdown(timeout = 0, block = true)
      @shutdown_semaphore.synchronize do
        return if @is_shutdown
        @is_shutdown = true
      end
      # nonblocking is intended for calling from within a Handler
      # but we can't change the order of operations here, so lets thread
      shutdown_proc = lambda do
        @io_manager.shutdown(timeout)
        @transport_semaphore.synchronize do
          @server_transport.close # this will break the accept loop
        end
      end
      if block
        shutdown_proc.call
      else
        Thread.new &shutdown_proc
      end
    end

    private

    def start_io_manager
      iom = IOManager.new(@processor, @server_transport, @transport_factory, @protocol_factory, @num_threads, @logger)
      iom.spawn
      iom
    end

    class IOManager # :nodoc:
      DEFAULT_BUFFER = 2**20

      def initialize(processor, server_transport, transport_factory, protocol_factory, num, logger)
        @processor = processor
        @server_transport = server_transport
        @transport_factory = transport_factory
        @protocol_factory = protocol_factory
        @num_threads = num
        @logger = logger
        @connections = []
        @readable_connections = []
        @buffers = Hash.new { |h, k| h[k] = Bytes.empty_byte_buffer }
        @response_queues = {}
        @signal_queue = Queue.new
        @signal_pipes = IO.pipe
        @signal_pipes[1].sync = true
        @worker_queue = Queue.new
        @shutdown_queue = Queue.new
        @capacity_callback = method(:response_capacity_available)
        @error_callback = method(:response_write_failed)
        @full_callback = method(:pause_connection)
      end

      def add_connection(socket)
        signal [:connection, socket]
      end

      def spawn
        @iom_thread = Thread.new do
          @logger.debug "Starting #{self}"
          run
        end
      end

      def shutdown(timeout = 0)
        @logger.debug "#{self} is shutting down workers"
        @worker_queue.clear
        @num_threads.times { @worker_queue.push [:shutdown] }
        signal [:shutdown, timeout]
        @shutdown_queue.pop
        @signal_pipes[0].close
        @signal_pipes[1].close
        @logger.debug "#{self} is shutting down, goodbye"
      end

      def ensure_closed
        kill_worker_threads if @worker_threads
        if @iom_thread&.alive?
          @iom_thread.kill
          @iom_thread.join
        end
        close_connections
        close_signal_pipes
      end

      private

      def run
        spin_worker_threads

        loop do
          rd = select_readable
          if rd.delete @signal_pipes[0]
            break if read_signals == :shutdown
          end
          rd.each do |fd|
            begin
              if fd.handle.eof?
                remove_connection fd
              else
                read_connection fd
              end
            rescue IOError, SystemCallError, TransportException => e
              @logger.debug "#{self} could not read connection: #{e.inspect}"
              remove_connection fd
            end
          end
        end
        join_worker_threads(@shutdown_timeout)
      ensure
        @shutdown_queue.push :shutdown
      end

      def select_readable
        select([@signal_pipes[0], *@readable_connections]).first
      rescue IOError, SystemCallError, TransportException => e
        @logger.debug "#{self} discarded closed connections after select failed: #{e.inspect}"
        raise if remove_closed_connections == 0

        retry
      end

      def remove_closed_connections
        closed = @connections.select do |fd|
          @response_queues[fd]&.closed? || !fd.open?
        rescue IOError, SystemCallError, TransportException
          true
        end
        closed.each { |fd| remove_connection(fd) }
        closed.length
      end

      def read_connection(fd)
        @buffers[fd] << fd.read(DEFAULT_BUFFER)
        dispatch_frames(fd)
      end

      def dispatch_frames(fd)
        response_queue = @response_queues[fd]
        return unless response_queue

        while (frame_size = complete_frame_size(@buffers[fd]))
          sequence = response_queue.reserve
          break if sequence.nil?

          @logger.debug "#{self} is processing a frame"
          frame = @buffers[fd].slice!(0, frame_size)
          @worker_queue.push [:frame, response_queue, sequence, frame]
        end
      end

      def spin_worker_threads
        @logger.debug "#{self} is spinning up worker threads"
        @worker_threads = []
        @num_threads.times do
          @worker_threads << spin_thread
        end
      end

      def spin_thread
        Worker.new(@processor, @transport_factory, @protocol_factory, @logger, @worker_queue).spawn
      end

      def signal(msg)
        @signal_queue << msg
        @signal_pipes[1].write " "
      end

      def read_signals
        # clear the signal pipe
        # note that since read_nonblock is broken in jruby,
        # we can only read up to a set number of signals at once
        sigstr = @signal_pipes[0].readpartial(1024)
        # now read the signals
        begin
          sigstr.length.times do
            signal, obj = @signal_queue.pop(true)
            case signal
            when :connection
              @connections << obj
              @readable_connections << obj
              @response_queues[obj] = new_response_queue(obj)
            when :capacity
              if @response_queues.key?(obj)
                @readable_connections << obj unless @readable_connections.include?(obj)
                dispatch_frames(obj)
              end
            when :disconnect
              remove_connection(obj)
            when :shutdown
              @shutdown_timeout = obj
              return :shutdown
            end
          end
        rescue ThreadError
          # out of signals
          # note that in a perfect world this would never happen, since we're
          # only reading the number of signals pushed on the pipe, but given the lack
          # of locks, in theory we could clear the pipe/queue while a new signal is being
          # placed on the pipe, at which point our next read_signals would hit this error
        end
      end

      def remove_connection(fd)
        @connections.delete fd
        @readable_connections.delete fd
        @buffers.delete fd
        @response_queues.delete(fd)&.close
        fd.close
      rescue IOError, SystemCallError, TransportException
      end

      def new_response_queue(fd)
        ResponseQueue.new(
          fd,
          @logger,
          max_in_flight: [@num_threads, 1].max,
          on_full: @full_callback,
          on_capacity: @capacity_callback,
          on_error: @error_callback
        )
      end

      def response_capacity_available(fd)
        signal [:capacity, fd]
      rescue IOError, SystemCallError
      end

      def response_write_failed(fd, _error)
        signal [:disconnect, fd]
      rescue IOError, SystemCallError
      end

      def pause_connection(fd)
        @readable_connections.delete(fd)
      end

      def join_worker_threads(shutdown_timeout)
        start = Time.now
        @worker_threads.each do |t|
          if shutdown_timeout > 0
            timeout = (start + shutdown_timeout) - Time.now
            break if timeout <= 0
            t.join(timeout)
          else
            t.join
          end
        end
        kill_worker_threads
      end

      def kill_worker_threads
        @worker_threads.each do |t|
          t.kill if t.status
        end
        @worker_threads.clear
      end

      def close_connections
        @connections.dup.each { |fd| remove_connection(fd) }
        @response_queues.each_value(&:close)
        @response_queues.clear
        @connections.clear
        @readable_connections.clear
        @buffers.clear
      end

      def close_signal_pipes
        @signal_pipes.each do |pipe|
          begin
            pipe.close unless pipe.closed?
          rescue IOError
          end
        end
      end

      def complete_frame_size(buf)
        return if buf.length < 4

        frame_size = buf.unpack1('N') + 4
        frame_size if buf.length >= frame_size
      end

      class ResponseBufferTransport < BaseTransport # :nodoc:
        def initialize
          reset
        end

        def reset
          @data = nil
        end

        def write(buf, size = nil)
          chunk = if size && size < buf.bytesize
            buf.byteslice(0, size)
          else
            buf
          end
          if @data.nil?
            @data = chunk
          elsif @data.is_a?(Array)
            @data << chunk
          else
            @data = [@data, chunk]
          end
        end

        def flush; end

        def data
          case @data
          when nil
            Bytes.empty_byte_buffer
          when Array
            @data.join
          else
            @data
          end
        end

        def take
          response = data
          reset
          response
        end
      end

      class ResponseQueue # :nodoc:
        # A connection owns one queue. It bounds outstanding responses and publishes
        # only contiguous request sequences to the connection's transport.
        def initialize(transport, logger, max_in_flight: nil, on_full: nil, on_capacity: nil, on_error: nil)
          if max_in_flight && max_in_flight < 1
            raise ArgumentError, 'max_in_flight must be at least 1'
          end

          @transport = transport
          @logger = logger
          @max_in_flight = max_in_flight
          @on_full = on_full
          @on_capacity = on_capacity
          @on_error = on_error
          @mutex = Mutex.new
          @next_sequence = 0
          @next_to_publish = 0
          @in_flight = 0
          @completed = nil
          @closed = false
        end

        def reserve
          became_full = false
          sequence = @mutex.synchronize do
            return unless accepting_without_lock?

            sequence = @next_sequence
            @next_sequence += 1
            @in_flight += 1
            became_full = full?
            sequence
          end
          @on_full&.call(@transport) if became_full
          sequence
        end

        def closed?
          @mutex.synchronize { @closed }
        end

        def complete(sequence, response)
          error = nil
          capacity_available = false

          @mutex.synchronize do
            return if @closed || sequence < @next_to_publish

            was_full = full?
            begin
              publish_response(sequence, response)
              capacity_available = was_full && accepting_without_lock?
            rescue IOError, SystemCallError, TransportException => e
              @logger.debug "#{self} could not write response: #{e.inspect}"
              close_without_lock
              error = e
            end
          end

          if error
            @on_error&.call(@transport, error)
          elsif capacity_available
            @on_capacity&.call(@transport)
          end
        end

        def close
          @mutex.synchronize { close_without_lock }
        end

        private

        def accepting_without_lock?
          !@closed && !full?
        end

        def full?
          @max_in_flight && @in_flight >= @max_in_flight
        end

        def close_without_lock
          @closed = true
          @in_flight = 0
          @completed = nil
        end

        def publish_response(sequence, response)
          unless sequence == @next_to_publish
            (@completed ||= {})[sequence] = response
            return
          end

          write_response(response)
          while @completed&.key?(@next_to_publish)
            response = @completed.delete(@next_to_publish)
            write_response(response)
          end
          @completed = nil if @completed&.empty?
        end

        def write_response(response)
          unless response.nil? || response.empty?
            @transport.write(response)
            @transport.flush
          end
          @next_to_publish += 1
          @in_flight -= 1
        end
      end

      class OnewayAwareProtocol < BaseProtocol # :nodoc:
        include ProtocolDecorator

        def initialize
          @protocol = nil
          @oneway = false
        end

        def reset(protocol, response_queue, sequence)
          @protocol = protocol
          @response_queue = response_queue
          @sequence = sequence
          @oneway = false
        end

        def read_message_begin
          message_begin = @protocol.read_message_begin
          if !@oneway && message_begin[1] == MessageTypes::ONEWAY
            @oneway = true
            @response_queue.complete(@sequence, nil)
          end
          message_begin
        end

        def oneway?
          @oneway
        end
      end

      class Worker # :nodoc:
        def initialize(processor, transport_factory, protocol_factory, logger, queue)
          @processor = processor
          @transport_factory = transport_factory
          @protocol_factory = protocol_factory
          @logger = logger
          @queue = queue
          @response = ResponseBufferTransport.new
          @request_protocol = OnewayAwareProtocol.new
        end

        def spawn
          Thread.new do
            @logger.debug "#{self} is spawning"
            run
          end
        end

        private

        def run
          loop do
            cmd, *args = @queue.pop
            case cmd
            when :shutdown
              @logger.debug "#{self} is shutting down, goodbye"
              break
            when :frame
              response_queue, sequence, frame = args
              next if response_queue.closed?

              @response.reset
              @request_protocol.reset(nil, response_queue, sequence)
              begin
                otrans = @transport_factory.get_transport(@response)
                oprot = @protocol_factory.get_protocol(otrans)
                membuf = MemoryBufferTransport.new(frame)
                itrans = @transport_factory.get_transport(membuf)
                iprot = @protocol_factory.get_protocol(itrans)
                @request_protocol.reset(iprot, response_queue, sequence)
                @processor.process(@request_protocol, oprot)
              rescue => e
                @logger.error "#{Thread.current.inspect} raised error: #{e.inspect}\n#{e.backtrace.join("\n")}"
              ensure
                if @request_protocol.oneway?
                  @response.reset
                else
                  response_queue.complete(sequence, @response.take)
                end
              end
            end
          end
        end
      end
    end
  end
end
