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

require "logger"
require "thread"

module Thrift
  # this class expects to always use a FramedTransport for reading messages
  class NonblockingServer < BaseServer
    def initialize(processor, server_transport, transport_factory = nil, protocol_factory = nil, num = 20, logger = nil,
                   max_frame_size: FramedTransport::DEFAULT_MAX_FRAME_SIZE)
      super(processor, server_transport, transport_factory, protocol_factory)
      unless max_frame_size > 0 && max_frame_size <= FramedTransport::HARD_MAX_FRAME_SIZE
        raise ArgumentError, "max_frame_size must be > 0 and <= #{FramedTransport::HARD_MAX_FRAME_SIZE}"
      end
      @num_threads = num
      @max_frame_size = max_frame_size
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
        accept_clients do |socket|
          @logger.debug "Accepted socket: #{socket.inspect}"
          @io_manager.add_connection socket
        end
      rescue IOError
      end
      # without the IO thread no connection would be read
      raise @io_manager.error if @io_manager.error
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
        Thread.new do
          Thread.current.name = "thrift.shutdown"
          shutdown_proc.call
        end
      end
    end

    private

    # Waits until a client is ready to be accepted, and returns false once the
    # server is shutting down or the IO manager has failed.
    def wait_for_client
      until @server_transport.closed? || @io_manager.error
        return true if select([@server_transport], nil, nil, 0.1)
      end
      false
    rescue Errno::EBADF
      # Closing @server_transport in shutdown can make select raise EBADF.
      false
    end

    def start_io_manager
      iom = IOManager.new(
        @processor, @server_transport, @transport_factory, @protocol_factory, @num_threads, @logger,
        max_frame_size: @max_frame_size,
      )
      iom.spawn
      iom
    end

    class IOManager # :nodoc:
      DEFAULT_BUFFER = 2**20

      def initialize(processor, server_transport, transport_factory, protocol_factory, num, logger,
                     max_frame_size: FramedTransport::DEFAULT_MAX_FRAME_SIZE)
        @processor = processor
        @server_transport = server_transport
        @transport_factory = transport_factory
        @protocol_factory = protocol_factory
        @num_threads = num
        @logger = logger
        @max_frame_size = max_frame_size
        @connections = []
        @buffers = Hash.new { |h, k| h[k] = Bytes.empty_byte_buffer }
        # Frames of each connection that the workers have not finished.
        @frames_in_progress = Hash.new(0)
        # Connections whose stream ended before the workers finished their frames.
        @ended_connections = {}
        @frames_lock = Mutex.new
        @signal_queue = Queue.new
        @signal_pipes = IO.pipe
        @signal_pipes[1].sync = true
        @worker_queue = Queue.new
        @shutdown_queue = Queue.new
      end

      def add_connection(socket)
        signal [:connection, socket]
      end

      # The error that stopped the IO thread, if one did.
      attr_reader :error

      def spawn
        @iom_thread = Thread.new do
          Thread.current.name = "thrift.io"
          @logger.debug "Starting #{self}"
          run
        rescue => e
          @logger.error "#{self} stopped: #{e.inspect}\n#{e.backtrace.join("\n")}"
          @error = e
        end
      end

      def shutdown(timeout = 0)
        @logger.debug "#{self} is shutting down workers"
        # An interrupt from another thread, such as a Timeout around shutdown,
        # waits until the discarded frames are accounted for, so that it
        # cannot leave their connections open.
        Thread.handle_interrupt(Object => :never) do
          discarded = take_queued_frames
          # Frames the IO thread queues after these wait unstarted until it
          # discards them.
          @num_threads.times { @worker_queue.push [:shutdown] }
          signal [:shutdown, timeout]
          # After the signal, so that closing connections does not delay it.
          discarded.each { |fd| frame_done(fd) }
        end
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
          # A worker closes its connection when writing a reply fails.
          @connections.reject(&:open?).each { |fd| remove_connection fd }
          begin
            rd, = select([@signal_pipes[0], *@connections])
          rescue IOError, Errno::EBADF
            # A worker closed a connection while select waited on it. Drop it and
            # select again; an error with every connection open is not that.
            raise if @connections.all?(&:open?)
            next
          end
          if rd.delete @signal_pipes[0]
            break if read_signals == :shutdown
          end
          rd.each do |fd|
            read_connection fd
          rescue TransportException
            remove_connection fd
          end
        end
        # This thread queues every frame, so none arrives after this. Taking
        # them also takes the stop commands shutdown queued, so queue new ones.
        take_queued_frames.each { |fd| frame_done(fd) }
        @num_threads.times { @worker_queue.push [:shutdown] }
        join_worker_threads(@shutdown_timeout)
      ensure
        @shutdown_queue.push :shutdown
      end

      def read_connection(fd)
        # Read only what has arrived: a blocking read on a TLS connection waits
        # for the rest of a record and holds up every other connection.
        data = fd.read_nonblock(DEFAULT_BUFFER)
        return end_connection(fd) if data.nil?
        return if data.is_a?(Symbol) # :wait_readable or :wait_writable
        buf = @buffers[fd]
        buf << data
        while buf.length >= 4
          # Refuse the size before collecting the frame: the four-byte length
          # is read here and the bytes are then buffered as they arrive, for as
          # long as the declared length allows. FramedTransport applies the
          # same maximum to the frame the worker later reads back.
          size = buf.unpack1("N")
          if size > @max_frame_size
            @logger.error "Frame size #{size} exceeds maximum #{@max_frame_size}, closing connection"
            fd.close
            remove_connection fd
            break
          end
          frame = slice_frame!(buf)
          break if frame.nil?
          @logger.debug "#{self} is processing a frame"
          @frames_lock.synchronize { @frames_in_progress[fd] += 1 }
          @worker_queue.push [:frame, fd, frame]
        end
      end

      def spin_worker_threads
        @logger.debug "#{self} is spinning up worker threads"
        @worker_threads = []
        @num_threads.times do |index|
          @worker_threads << spin_thread(index)
        end
      end

      def spin_thread(index)
        Worker.new(
          @processor, @transport_factory, @protocol_factory, @logger, @worker_queue, method(:frame_done),
        ).spawn(index)
      end

      # Stops reading a connection whose stream has ended, and closes it once
      # the workers have written the replies to the frames already read.
      def end_connection(fd)
        # Before removing it, so that a connection being ended is always in
        # one of the lists close_connections closes.
        @frames_lock.synchronize do
          if @frames_in_progress.key?(fd)
            @ended_connections[fd] = true
          else
            fd.close
          end
        end
        remove_connection fd
      end

      # Empties the worker queue and returns the connection of each frame no
      # worker has started. Passing each to frame_done closes a connection
      # whose stream has ended now, and not only once the running frames
      # finish.
      def take_queued_frames
        fds = []
        loop do
          cmd, fd, = @worker_queue.pop(true)
          fds << fd if cmd == :frame
        rescue ThreadError # the queue is empty
          return fds
        end
      end

      # Called by a worker once it has processed a frame of the connection.
      def frame_done(fd)
        @frames_lock.synchronize do
          next if (@frames_in_progress[fd] -= 1) > 0

          @frames_in_progress.delete fd
          fd.close if @ended_connections.delete fd
        end
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
        # don't explicitly close it, a thread may still be writing to it
        @connections.delete fd
        @buffers.delete fd
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
        # A killed worker can still report its frame while this runs.
        @frames_lock.synchronize do
          [*@connections, *@ended_connections.keys].each do |fd|
            fd.close
          rescue IOError, SystemCallError, TransportException
          end
          @connections.clear
          @ended_connections.clear
          # Counts no worker will report: frames still queued when the IO
          # thread failed, and frames of workers killed before reporting.
          @frames_in_progress.clear
        end
        @buffers.clear
      end

      def close_signal_pipes
        @signal_pipes.each do |pipe|
          pipe.close unless pipe.closed?
        rescue IOError
        end
      end

      def slice_frame!(buf)
        if buf.length >= 4
          size = buf.unpack1("N")
          if buf.length >= size + 4
            buf.slice!(0, size + 4)
          else
            nil
          end
        else
          nil
        end
      end

      class Worker # :nodoc:
        def initialize(processor, transport_factory, protocol_factory, logger, queue, frame_done)
          @processor = processor
          @transport_factory = transport_factory
          @protocol_factory = protocol_factory
          @logger = logger
          @queue = queue
          @frame_done = frame_done
        end

        def spawn(index)
          Thread.new(index) do |worker_id|
            Thread.current.name = "thrift.nb.#{worker_id}"
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
              fd, frame = args
              begin
                otrans = @transport_factory.get_transport(fd)
                oprot = @protocol_factory.get_protocol(otrans)
                membuf = MemoryBufferTransport.new(frame)
                itrans = @transport_factory.get_transport(membuf)
                iprot = @protocol_factory.get_protocol(itrans)
                @processor.process(iprot, oprot)
              rescue StandardError, ScriptError, SystemStackError => e
                @logger.error "#{Thread.current.inspect} raised error: #{e.inspect}\n#{e.backtrace.join("\n")}"
              ensure
                @frame_done.call(fd)
              end
            end
          end
        end
      end
    end
  end
end
