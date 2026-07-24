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

require 'spec_helper'
require 'timeout'

describe 'NonblockingServer' do
  class Handler
    def initialize
      @queue = Queue.new
      @block_started = Queue.new
    end

    attr_accessor :server
    attr_reader :block_started

    def greeting(english)
      if english
        SpecNamespace::Hello.new
      else
        SpecNamespace::Hello.new(:greeting => "Aloha!")
      end
    end

    def block
      @block_started << true
      @queue.pop
    end

    def unblock(n)
      n.times { @queue.push true }
    end

    def sleep(time)
      Kernel.sleep time
    end

    def shutdown
      @server.shutdown(0, false)
    end
  end

  class SpecProcessor
    def initialize(processor, finished)
      @processor = processor
      @finished = finished
    end

    def process(iprot, oprot)
      @processor.process(iprot, oprot)
    ensure
      @finished << true
    end
  end

  class BlockingProcessor
    def initialize(started, release)
      @started = started
      @release = release
    end

    def process(iprot, _oprot)
      @started << iprot.read_message_begin
      @release.pop
    end
  end

  class SpecTransport < Thrift::BaseTransport
    def initialize(transport, queue, write_queue = nil)
      @transport = transport
      @queue = queue
      @write_queue = write_queue
      @flushed = false
    end

    def open?
      @transport.open?
    end

    def open
      @transport.open
    end

    def close
      @transport.close
    end

    def read(sz)
      @transport.read(sz)
    end

    def write(buf, sz = nil)
      data = sz ? buf[0...sz] : buf
      @write_queue << data.dup if @write_queue
      @transport.write(data)
    end

    def flush
      @queue.push :flushed unless @flushed or @queue.nil?
      @flushed = true
      @transport.flush
    end

    def handle
      @transport.handle
    end

    def to_io
      @transport.to_io
    end
  end

  class SpecServerSocket < Thrift::ServerSocket
    def initialize(host, port, queue, write_queue = nil)
      super(host, port)
      @queue = queue
      @write_queue = write_queue
    end

    def listen
      super
      @queue.push :listen
    end

    def accept
      transport = super
      SpecTransport.new(transport, nil, @write_queue)
    end
  end

  describe Thrift::NonblockingServer do
    before(:each) do
      @port = available_port
      @handler = Handler.new
      @processor_finished = Queue.new
      processor = SpecProcessor.new(SpecNamespace::NonblockingService::Processor.new(@handler), @processor_finished)
      queue = Queue.new
      @server_writes = Queue.new
      @transport = SpecServerSocket.new('localhost', @port, queue, @server_writes)
      transport_factory = Thrift::FramedTransportFactory.new
      logger = Logger.new(STDERR)
      logger.level = Logger::WARN
      @server = Thrift::NonblockingServer.new(processor, @transport, transport_factory, nil, 5, logger)
      @handler.server = @server
      @server_thread = Thread.new(Thread.current) do |master_thread|
        begin
          @server.serve
        rescue => e
          p e
          puts e.backtrace * "\n"
          master_thread.raise e
        end
      end
      queue.pop
      wait_until_listening(@transport, @server_thread)

      @clients = []
      @catch_exceptions = false
    end

    after(:each) do
      @clients.each { |client, trans| trans.close }
      @server.shutdown(1, false) if @server
      @server_thread.join(2) if @server_thread
      @server_thread.kill if @server_thread && @server_thread.alive?
      @server_thread.join(2) if @server_thread
      @transport.close if @transport
    end

    def setup_client(queue = nil)
      transport = SpecTransport.new(Thrift::FramedTransport.new(Thrift::Socket.new('localhost', @port)), queue)
      protocol = Thrift::BinaryProtocol.new(transport)
      client = SpecNamespace::NonblockingService::Client.new(protocol)
      transport.open
      @clients << [client, transport]
      client
    end

    def setup_client_thread(result)
      queue = Queue.new
      Thread.new do
        begin
          client = setup_client
          while (cmd = queue.pop)
            msg, *args = cmd
            case msg
            when :block
              result << client.block
            when :unblock
              client.unblock(args.first)
            when :hello
              result << client.greeting(true) # ignore result
            when :sleep
              client.sleep(args[0] || 0.5)
              result << :slept
            when :shutdown
              client.shutdown
            when :exit
              result << :done
              break
            end
          end
          @clients.each { |c, t| t.close and break if c == client } # close the transport
        rescue => e
          raise e unless @catch_exceptions
        end
      end
      queue
    end

    def reply_sequence_id(reply)
      transport = Thrift::FramedTransport.new(Thrift::MemoryBufferTransport.new(reply))
      _name, _type, sequence_id = Thrift::BinaryProtocol.new(transport).read_message_begin
      sequence_id
    end

    it "should handle basic message passing" do
      client = setup_client
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      expect(client.greeting(false)).to eq(SpecNamespace::Hello.new(:greeting => 'Aloha!'))
      @server.shutdown
    end

    it "should handle concurrent clients" do
      queue = Queue.new
      trans_queue = Queue.new
      4.times do
        Thread.new(Thread.current) do |main_thread|
          begin
            queue.push setup_client(trans_queue).block
          rescue => e
            main_thread.raise e
          end
        end
      end
      4.times { trans_queue.pop }
      setup_client.unblock(4)
      4.times { expect(queue.pop).to be_truthy }
      @server.shutdown
    end

    it "publishes replies for one connection in request order" do
      client = setup_client

      client.send_block
      expect(@handler.block_started.pop).to be_truthy

      client.send_greeting(true)
      expect(@processor_finished.pop).to be_truthy

      expect do
        @server_writes.pop(true)
      end.to raise_error(ThreadError)

      @handler.unblock(1)
      replies = 2.times.map { Timeout.timeout(1) { @server_writes.pop } }

      expect(replies.map { |reply| reply_sequence_id(reply) }).to eq([0, 1])
      expect(client.recv_block).to be_truthy
      expect(client.recv_greeting).to eq(SpecNamespace::Hello.new)
    end

    it "should handle messages from more than 5 long-lived connections" do
      queues = []
      result = Queue.new
      7.times do |i|
        queues << setup_client_thread(result)
        Thread.pass if i == 4 # give the server time to accept connections
      end
      client = setup_client
      # block 4 connections
      4.times { |i| queues[i] << :block }
      queues[4] << :hello
      queues[5] << :hello
      queues[6] << :hello
      3.times { expect(result.pop).to eq(SpecNamespace::Hello.new) }
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      queues[5] << [:unblock, 4]
      4.times { expect(result.pop).to be_truthy }
      queues[2] << :hello
      expect(result.pop).to eq(SpecNamespace::Hello.new)
      expect(client.greeting(false)).to eq(SpecNamespace::Hello.new(:greeting => 'Aloha!'))
      7.times { queues.shift << :exit }
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      @server.shutdown
    end

    it "should shut down when asked" do
      # connect first to ensure it's running
      client = setup_client
      client.greeting(false) # force a message pass
      @server.shutdown
      expect(@server_thread.join(2)).to be_an_instance_of(Thread)
    end

    it "should continue processing active messages when shutting down" do
      result = Queue.new
      client = setup_client_thread(result)
      client << :sleep
      sleep 0.1 # give the server time to start processing the client's message
      @server.shutdown
      expect(@server_thread.join(2)).to be_an_instance_of(Thread)
      expect(result.pop).to eq(:slept)
    end

    it "should kill active messages when they don't expire while shutting down" do
      result = Queue.new
      client = setup_client_thread(result)
      client << [:sleep, 10.0]
      sleep 0.1 # start processing the client's message
      @server.shutdown(1)
      @catch_exceptions = true
      expect(@server_thread.join(3)).not_to be_nil
      expect(result).to be_empty
    end

    it "should allow shutting down in response to a message" do
      client = setup_client
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      client.shutdown
      expect(@server_thread.join(2)).not_to be_nil
    end
  end

  describe Thrift::NonblockingServer::IOManager do
    def build_io_manager(num_threads: 1)
      logger = Logger.new(IO::NULL)
      logger.level = Logger::FATAL
      Thrift::NonblockingServer::IOManager.new(
        double('processor'),
        double('server_transport'),
        Thrift::BaseTransportFactory.new,
        Thrift::BinaryProtocolFactory.new,
        num_threads,
        logger
      )
    end

    def build_response_queue(max_in_flight: nil, on_capacity: nil, on_error: nil)
      transport = double('transport', :write => nil, :flush => nil, :close => nil)
      logger = Logger.new(IO::NULL)
      logger.level = Logger::FATAL
      response_queue = Thrift::NonblockingServer::IOManager::ResponseQueue.new(
        transport,
        logger,
        max_in_flight: max_in_flight,
        on_capacity: on_capacity,
        on_error: on_error
      )
      [response_queue, transport]
    end

    def framed_message(type)
      output = Thrift::MemoryBufferTransport.new
      protocol = Thrift::BinaryProtocol.new(Thrift::FramedTransport.new(output))
      protocol.write_message_begin('request', type, 0)
      protocol.write_struct_begin('args')
      protocol.write_field_stop
      protocol.write_struct_end
      protocol.write_message_end
      protocol.trans.flush
      output.read(output.available)
    end

    describe Thrift::NonblockingServer::IOManager::ResponseBufferTransport do
      it "returns a single complete write without copying it" do
        transport = described_class.new
        response = +"response"

        transport.write(response)

        expect(transport.data).to be(response)
      end

      it "combines partial and repeated writes" do
        transport = described_class.new

        transport.write('first', 3)
        transport.write('second')

        expect(transport.data).to eq('firsecond')
      end

      it "can be reused without retaining its previous response" do
        transport = described_class.new
        transport.write('first response')

        expect(transport.take).to eq('first response')
        expect(transport.data).to be_empty

        transport.write('second response')

        expect(transport.data).to eq('second response')
      end
    end

    it "closes tracked connections and signal pipes during forced cleanup" do
      io_manager = build_io_manager
      connection = double('connection', :close => nil)
      pipe_a = double('pipe_a', :closed? => false, :close => nil)
      pipe_b = double('pipe_b', :closed? => false, :close => nil)

      io_manager.instance_variable_set(:@connections, [connection])
      io_manager.instance_variable_set(:@buffers, { connection => 'frame' })
      io_manager.instance_variable_set(:@signal_pipes, [pipe_a, pipe_b])
      io_manager.instance_variable_set(:@worker_threads, [])

      io_manager.ensure_closed

      expect(connection).to have_received(:close)
      expect(pipe_a).to have_received(:close)
      expect(pipe_b).to have_received(:close)
      expect(io_manager.instance_variable_get(:@connections)).to be_empty
      expect(io_manager.instance_variable_get(:@buffers)).to be_empty
    end

    it "continues closing remaining signal pipes when one close raises" do
      io_manager = build_io_manager
      pipe_a = double('pipe_a', :closed? => false)
      pipe_b = double('pipe_b', :closed? => false, :close => nil)

      allow(pipe_a).to receive(:close).and_raise(IOError)

      io_manager.instance_variable_set(:@signal_pipes, [pipe_a, pipe_b])
      io_manager.instance_variable_set(:@worker_threads, [])

      io_manager.send(:close_signal_pipes)

      expect(pipe_a).to have_received(:close)
      expect(pipe_b).to have_received(:close)
    end

    it "drops removed connections from bookkeeping" do
      io_manager = build_io_manager
      connection = double('connection', :close => nil)
      response_queue, response_transport = build_response_queue
      first_sequence = response_queue.reserve
      second_sequence = response_queue.reserve
      response_queue.complete(second_sequence, 'later reply')

      io_manager.instance_variable_set(:@connections, [connection])
      io_manager.instance_variable_set(:@buffers, { connection => 'frame' })
      io_manager.instance_variable_set(:@response_queues, { connection => response_queue })

      io_manager.send(:remove_connection, connection)
      response_queue.complete(first_sequence, 'first reply')

      expect(io_manager.instance_variable_get(:@connections)).to be_empty
      expect(io_manager.instance_variable_get(:@buffers)).to be_empty
      expect(io_manager.instance_variable_get(:@response_queues)).to be_empty
      expect(response_transport).not_to have_received(:write)
      expect(response_transport).not_to have_received(:flush)
    end

    it "removes and closes a connection after its response write fails" do
      io_manager = build_io_manager
      connection = double('connection', :write => nil, :flush => nil, :close => nil)
      allow(connection).to receive(:write).and_raise(IOError, 'closed stream')
      response_queue = io_manager.send(:new_response_queue, connection)

      io_manager.instance_variable_set(:@connections, [connection])
      io_manager.instance_variable_set(:@buffers, { connection => '' })
      io_manager.instance_variable_set(:@response_queues, { connection => response_queue })

      response_queue.complete(response_queue.reserve, 'reply')
      io_manager.send(:read_signals)

      expect(connection).to have_received(:close).once
      expect(io_manager.instance_variable_get(:@connections)).to be_empty
      expect(io_manager.instance_variable_get(:@buffers)).to be_empty
      expect(io_manager.instance_variable_get(:@response_queues)).to be_empty
    end

    it "discards a closed connection when select observes its stale descriptor" do
      io_manager = build_io_manager
      connection = double('connection', :open? => false, :close => nil)
      allow(connection).to receive(:to_io).and_raise(IOError, 'closed stream')
      response_queue = io_manager.send(:new_response_queue, connection)

      io_manager.instance_variable_set(:@connections, [connection])
      io_manager.instance_variable_set(:@readable_connections, [connection])
      io_manager.instance_variable_set(:@buffers, { connection => '' })
      io_manager.instance_variable_set(:@response_queues, { connection => response_queue })
      io_manager.send(:signal, [:noop, nil])

      begin
        expect(io_manager.send(:select_readable)).to eq([io_manager.instance_variable_get(:@signal_pipes)[0]])
        io_manager.send(:read_signals)

        expect(connection).to have_received(:close).once
        expect(io_manager.instance_variable_get(:@connections)).to be_empty
        expect(io_manager.instance_variable_get(:@readable_connections)).to be_empty
      ensure
        io_manager.ensure_closed
      end
    end

    it "keeps excess frames buffered until the connection has response capacity" do
      io_manager = build_io_manager(num_threads: 2)
      connection = double('connection', :write => nil, :flush => nil, :close => nil)
      response_queue = io_manager.send(:new_response_queue, connection)
      frame = [1].pack('N') << 'x'

      io_manager.instance_variable_set(:@connections, [connection])
      io_manager.instance_variable_set(:@buffers, { connection => frame * 3 })
      io_manager.instance_variable_set(:@response_queues, { connection => response_queue })

      io_manager.send(:dispatch_frames, connection)

      expect(io_manager.instance_variable_get(:@worker_queue).size).to eq(2)
      expect(io_manager.instance_variable_get(:@buffers)[connection]).to eq(frame)

      response_queue.complete(0, '')
      io_manager.send(:read_signals)

      expect(io_manager.instance_variable_get(:@worker_queue).size).to eq(3)
      expect(io_manager.instance_variable_get(:@buffers)[connection]).to be_empty
    end

    it "advances past oneway completions before publishing later replies" do
      response_queue, transport = build_response_queue
      oneway_sequence = response_queue.reserve
      reply_sequence = response_queue.reserve

      response_queue.complete(reply_sequence, 'later reply')
      expect(transport).not_to have_received(:write)

      response_queue.complete(oneway_sequence, '')

      expect(transport).to have_received(:write).with('later reply').once
      expect(transport).to have_received(:flush).once
    end

    it "does not let a running oneway handler block a later reply" do
      response_queue, transport = build_response_queue
      worker_queue = Queue.new
      started = Queue.new
      release = Queue.new
      worker = Thrift::NonblockingServer::IOManager::Worker.new(
        BlockingProcessor.new(started, release),
        Thrift::FramedTransportFactory.new,
        Thrift::BinaryProtocolFactory.new,
        Logger.new(IO::NULL),
        worker_queue
      )
      worker_thread = worker.spawn
      oneway_sequence = response_queue.reserve
      reply_sequence = response_queue.reserve

      begin
        worker_queue.push [:frame, response_queue, oneway_sequence, framed_message(Thrift::MessageTypes::ONEWAY)]
        expect(started.pop[1]).to eq(Thrift::MessageTypes::ONEWAY)

        response_queue.complete(reply_sequence, 'later reply')

        expect(transport).to have_received(:write).with('later reply').once
        expect(transport).to have_received(:flush).once
      ensure
        release << true
        worker_queue.push [:shutdown]
        worker_thread.join(1)
      end
    end

    it "notifies its owner and closes after a response write fails" do
      failures = []
      response_queue, transport = build_response_queue(
        on_error: ->(failed_transport, error) { failures << [failed_transport, error] }
      )
      allow(transport).to receive(:write).and_raise(IOError, 'closed stream')

      response_queue.complete(response_queue.reserve, 'reply')

      expect(failures.length).to eq(1)
      expect(failures.first[0]).to be(transport)
      expect(failures.first[1]).to be_a(IOError)
      expect(response_queue.reserve).to be_nil
    end

    it "bounds completed responses until the missing reply is published" do
      capacity = []
      response_queue, transport = build_response_queue(
        max_in_flight: 2,
        on_capacity: ->(available_transport) { capacity << available_transport }
      )
      first_sequence = response_queue.reserve
      second_sequence = response_queue.reserve

      expect(response_queue.reserve).to be_nil
      response_queue.complete(second_sequence, 'second reply')
      expect(response_queue.reserve).to be_nil

      response_queue.complete(first_sequence, 'first reply')

      expect(transport).to have_received(:write).with('first reply').ordered
      expect(transport).to have_received(:write).with('second reply').ordered
      expect(response_queue.reserve).to eq(2)
      expect(capacity).to eq([transport])
    end

    it "advances past worker exceptions before publishing later replies" do
      response_queue, transport = build_response_queue
      worker_queue = Queue.new
      logger = Logger.new(IO::NULL)
      logger.level = Logger::FATAL
      processor = double('processor')
      allow(processor).to receive(:process).and_raise(StandardError, 'boom')
      worker = Thrift::NonblockingServer::IOManager::Worker.new(
        processor,
        Thrift::FramedTransportFactory.new,
        Thrift::BinaryProtocolFactory.new,
        logger,
        worker_queue
      )
      worker_thread = worker.spawn
      failing_sequence = response_queue.reserve
      reply_sequence = response_queue.reserve

      worker_queue.push [:frame, response_queue, failing_sequence, framed_message(Thrift::MessageTypes::CALL)]
      worker_queue.push [:shutdown]
      expect(Timeout.timeout(1) { worker_thread.join }).to be_a(Thread)
      expect(processor).to have_received(:process).once

      response_queue.complete(reply_sequence, 'later reply')

      expect(transport).to have_received(:write).with('later reply').once
      expect(transport).to have_received(:flush).once
    end
  end

  describe "#{Thrift::NonblockingServer} with TLS transport" do
    before(:each) do
      @port = available_port
      handler = Handler.new
      processor = SpecNamespace::NonblockingService::Processor.new(handler)
      @transport = Thrift::SSLServerSocket.new('localhost', @port, create_server_ssl_context)
      transport_factory = Thrift::FramedTransportFactory.new
      logger = Logger.new(STDERR)
      logger.level = Logger::WARN
      @server = Thrift::NonblockingServer.new(processor, @transport, transport_factory, nil, 5, logger)
      handler.server = @server

      @server_thread = Thread.new(Thread.current) do |master_thread|
        begin
          @server.serve
        rescue => e
          master_thread.raise e
        end
      end

      @clients = []
      wait_until_listening(@transport, @server_thread)
    end

    after(:each) do
      @clients.each(&:close)
      @server.shutdown if @server
      @server_thread.join(2) if @server_thread
      @transport.close if @transport
    end

    it "should handle requests over TLS" do
      expect(@server_thread).to be_alive

      client = setup_tls_client
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)

      @server.shutdown
      expect(@server_thread.join(2)).to be_an_instance_of(Thread)
    end

    def setup_tls_client
      transport = Thrift::FramedTransport.new(
        Thrift::SSLSocket.new('localhost', @port, nil, create_client_ssl_context)
      )
      protocol = Thrift::BinaryProtocol.new(transport)
      client = SpecNamespace::NonblockingService::Client.new(protocol)
      transport.open
      @clients << transport
      client
    end

    def ssl_keys_dir
      File.expand_path('../../../test/keys', __dir__)
    end

    def create_server_ssl_context
      OpenSSL::SSL::SSLContext.new.tap do |ctx|
        ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
        if ctx.respond_to?(:min_version=) && OpenSSL::SSL.const_defined?(:TLS1_2_VERSION)
          ctx.min_version = OpenSSL::SSL::TLS1_2_VERSION
        end
        ctx.ca_file = File.join(ssl_keys_dir, 'CA.pem')
        ctx.cert = OpenSSL::X509::Certificate.new(File.read(File.join(ssl_keys_dir, 'server.crt')))
        ctx.cert_store = OpenSSL::X509::Store.new
        ctx.cert_store.add_file(File.join(ssl_keys_dir, 'client.pem'))
        ctx.key = OpenSSL::PKey::RSA.new(File.read(File.join(ssl_keys_dir, 'server.key')))
      end
    end

    def create_client_ssl_context
      OpenSSL::SSL::SSLContext.new.tap do |ctx|
        ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
        if ctx.respond_to?(:min_version=) && OpenSSL::SSL.const_defined?(:TLS1_2_VERSION)
          ctx.min_version = OpenSSL::SSL::TLS1_2_VERSION
        end
        ctx.ca_file = File.join(ssl_keys_dir, 'CA.pem')
        ctx.cert = OpenSSL::X509::Certificate.new(File.read(File.join(ssl_keys_dir, 'client.crt')))
        ctx.cert_store = OpenSSL::X509::Store.new
        ctx.cert_store.add_file(File.join(ssl_keys_dir, 'server.pem'))
        ctx.key = OpenSSL::PKey::RSA.new(File.read(File.join(ssl_keys_dir, 'client.key')))
      end
    end
  end

  def wait_until_listening(server_transport, server_thread)
    Timeout.timeout(2) do
      until server_transport.handle
        raise "Server thread exited unexpectedly" unless server_thread.alive?
        sleep 0.01
      end
    end
  end

  def available_port
    TCPServer.open('localhost', 0) { |server| server.addr[1] }
  end
end
