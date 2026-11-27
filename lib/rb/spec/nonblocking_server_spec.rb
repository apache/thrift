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

require "spec_helper"
require "timeout"

describe "NonblockingServer" do
  class Handler
    def initialize
      @queue = Queue.new
    end

    attr_accessor :server

    def greeting(english)
      if english
        SpecNamespace::Hello.new
      else
        SpecNamespace::Hello.new(greeting: "Aloha!")
      end
    end

    def block
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

  class SpecTransport < Thrift::BaseTransport
    def initialize(transport, queue)
      @transport = transport
      @queue = queue
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
      @transport.write(buf, sz)
    end

    def flush
      @queue.push :flushed unless @flushed or @queue.nil?
      @flushed = true
      @transport.flush
    end
  end

  class SpecServerSocket < Thrift::ServerSocket
    def initialize(host, port, queue)
      super(host, port)
      @queue = queue
    end

    def listen
      super
      @queue.push :listen
    end
  end

  describe Thrift::NonblockingServer do
    before(:each) do
      @port = available_port
      handler = Handler.new
      processor = SpecNamespace::NonblockingService::Processor.new(handler)
      queue = Queue.new
      @transport = SpecServerSocket.new("localhost", @port, queue)
      transport_factory = Thrift::FramedTransportFactory.new
      logger = Logger.new(STDERR)
      logger.level = Logger::WARN
      @server = Thrift::NonblockingServer.new(processor, @transport, transport_factory, nil, 5, logger)
      handler.server = @server
      @server_thread = Thread.new(Thread.current) do |master_thread|
        @server.serve
      rescue => e
        p e
        puts e.backtrace * "\n"
        master_thread.raise e
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
      transport = SpecTransport.new(Thrift::FramedTransport.new(Thrift::Socket.new("localhost", @port)), queue)
      protocol = Thrift::BinaryProtocol.new(transport)
      client = SpecNamespace::NonblockingService::Client.new(protocol)
      transport.open
      @clients << [client, transport]
      client
    end

    def setup_client_thread(result)
      queue = Queue.new
      Thread.new do
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
      queue
    end

    it "should handle basic message passing" do
      client = setup_client
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      expect(client.greeting(false)).to eq(SpecNamespace::Hello.new(greeting: "Aloha!"))
      @server.shutdown
    end

    it "should handle concurrent clients" do
      queue = Queue.new
      trans_queue = Queue.new
      4.times do
        Thread.new(Thread.current) do |main_thread|
          queue.push setup_client(trans_queue).block
        rescue => e
          main_thread.raise e
        end
      end
      4.times { trans_queue.pop }
      setup_client.unblock(4)
      4.times { expect(queue.pop).to be_truthy }
      @server.shutdown
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
      expect(client.greeting(false)).to eq(SpecNamespace::Hello.new(greeting: "Aloha!"))
      7.times { queues.shift << :exit }
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      @server.shutdown
    end

    it "should close a connection whose frame is over the maximum" do
      socket = TCPSocket.new("localhost", @port)
      begin
        socket.write([Thrift::FramedTransport::DEFAULT_MAX_FRAME_SIZE + 1].pack("N") + ("x" * 1024))
        closed = Timeout.timeout(5) do
          socket.read(1).nil?
        rescue Errno::ECONNRESET
          true
        end
        expect(closed).to be(true)
      ensure
        socket.close
      end
    end

    it "keeps answering other clients while one client has sent part of a frame" do
      partial_client = TCPSocket.new("localhost", @port)
      client = setup_client
      begin
        partial_client.write("\x00\x00\x00".b) # three of the four frame size bytes
        reply = Timeout.timeout(2) { client.greeting(true) }
        expect(reply).to eq(SpecNamespace::Hello.new)
      ensure
        partial_client.close
      end
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

  describe "NonblockingServer accept errors" do
    it "preserves the original error when OpenSSL is not loaded" do
      hide_const("OpenSSL")
      error = RuntimeError.new("plain accept failed")
      server_transport = double(
        "server transport",
        listen: nil,
        closed?: false,
        close: nil,
      )
      allow(server_transport).to receive(:accept).and_raise(error)
      io_manager = double("IOManager", ensure_closed: nil, error: nil)
      server = Thrift::NonblockingServer.new(
        double("processor"),
        server_transport,
        nil,
        nil,
        1,
        Logger.new(IO::NULL),
      )
      allow(server).to receive(:start_io_manager).and_return(io_manager)
      allow(server).to receive(:select).and_return([[server_transport], nil, nil])

      expect { server.serve }.to raise_error(error)
    end
  end

  describe "NonblockingServer max_frame_size" do
    def build_server(**options)
      Thrift::NonblockingServer.new(
        double("processor"), double("server_transport"), nil, nil, 1, Logger.new(IO::NULL),
        **options,
      )
    end

    it "is validated" do
      [0, -1, Thrift::FramedTransport::HARD_MAX_FRAME_SIZE + 1].each do |bad|
        expect { build_server(max_frame_size: bad) }.to raise_error(ArgumentError, /max_frame_size/)
      end
    end

    it "is handed to the IOManager" do
      server = build_server(max_frame_size: 100)
      allow(Thrift::NonblockingServer::IOManager).to receive(:new).and_return(double("IOManager", spawn: nil))

      server.send(:start_io_manager)

      expect(Thrift::NonblockingServer::IOManager).to have_received(:new)
        .with(anything, anything, anything, anything, 1, anything, max_frame_size: 100)
    end
  end

  describe Thrift::NonblockingServer::IOManager do
    def build_io_manager(**options)
      logger = Logger.new(IO::NULL)
      logger.level = Logger::FATAL
      Thrift::NonblockingServer::IOManager.new(
        double("processor"),
        double("server_transport"),
        Thrift::BaseTransportFactory.new,
        Thrift::BinaryProtocolFactory.new,
        1,
        logger,
        **options,
      )
    end

    it "closes tracked connections and signal pipes during forced cleanup" do
      io_manager = build_io_manager
      connection = double("connection", close: nil)
      pipe_a = double("pipe_a", closed?: false, close: nil)
      pipe_b = double("pipe_b", closed?: false, close: nil)

      ended = double("ended connection", close: nil)

      io_manager.instance_variable_set(:@connections, [connection])
      io_manager.instance_variable_set(:@buffers, { connection => "frame" })
      # frames still queued when the IO thread failed
      io_manager.instance_variable_get(:@frames_in_progress).merge!(connection => 1, ended => 1)
      io_manager.instance_variable_set(:@ended_connections, { ended => true })
      io_manager.instance_variable_set(:@signal_pipes, [pipe_a, pipe_b])
      io_manager.instance_variable_set(:@worker_threads, [])

      io_manager.ensure_closed

      expect(connection).to have_received(:close)
      expect(ended).to have_received(:close)
      expect(pipe_a).to have_received(:close)
      expect(pipe_b).to have_received(:close)
      expect(io_manager.instance_variable_get(:@connections)).to be_empty
      expect(io_manager.instance_variable_get(:@buffers)).to be_empty
      expect(io_manager.instance_variable_get(:@frames_in_progress)).to be_empty
      expect(io_manager.instance_variable_get(:@ended_connections)).to be_empty
    end

    it "continues closing remaining signal pipes when one close raises" do
      io_manager = build_io_manager
      pipe_a = double("pipe_a", closed?: false)
      pipe_b = double("pipe_b", closed?: false, close: nil)

      allow(pipe_a).to receive(:close).and_raise(IOError)

      io_manager.instance_variable_set(:@signal_pipes, [pipe_a, pipe_b])
      io_manager.instance_variable_set(:@worker_threads, [])

      io_manager.send(:close_signal_pipes)

      expect(pipe_a).to have_received(:close)
      expect(pipe_b).to have_received(:close)
    end

    it "drops removed connections from bookkeeping" do
      io_manager = build_io_manager
      connection = double("connection", close: nil)

      io_manager.instance_variable_set(:@connections, [connection])
      io_manager.instance_variable_set(:@buffers, { connection => "frame" })

      io_manager.send(:remove_connection, connection)

      expect(io_manager.instance_variable_get(:@connections)).to be_empty
      expect(io_manager.instance_variable_get(:@buffers)).to be_empty
    end

    # The IOManager reads the four-byte size of a frame itself and collects
    # that many bytes before a worker sees the frame, so the maximum has to be
    # applied here, to the declared size, and not only by the FramedTransport
    # the worker later reads the collected frame through.
    def connection_sending(bytes)
      double("connection", close: nil, read_nonblock: bytes)
    end

    def read_from(io_manager, connection)
      io_manager.instance_variable_set(:@connections, [connection])
      io_manager.send(:read_connection, connection)
    end

    def frames_queued(io_manager)
      queue = io_manager.instance_variable_get(:@worker_queue)
      Array.new(queue.size) { queue.pop }
    end

    it "closes a connection whose frame is over the default maximum, before collecting it" do
      io_manager = build_io_manager
      connection = connection_sending([Thrift::FramedTransport::DEFAULT_MAX_FRAME_SIZE + 1].pack("N") + ("x" * 64))

      read_from(io_manager, connection)

      expect(connection).to have_received(:close)
      expect(io_manager.instance_variable_get(:@connections)).to be_empty
      expect(io_manager.instance_variable_get(:@buffers)).to be_empty
      expect(frames_queued(io_manager)).to be_empty
    end

    it "closes a connection whose frame is over a given maximum, even once the whole frame has arrived" do
      io_manager = build_io_manager(max_frame_size: 100)
      connection = connection_sending([101].pack("N") + ("x" * 101))

      read_from(io_manager, connection)

      expect(connection).to have_received(:close)
      expect(io_manager.instance_variable_get(:@buffers)).to be_empty
      expect(frames_queued(io_manager)).to be_empty
    end

    it "still hands a frame at the maximum to the workers" do
      io_manager = build_io_manager(max_frame_size: 100)
      frame = [100].pack("N") + ("y" * 100)
      connection = connection_sending(frame)

      read_from(io_manager, connection)

      expect(connection).not_to have_received(:close)
      expect(io_manager.instance_variable_get(:@connections)).to eq([connection])
      expect(frames_queued(io_manager)).to eq([[:frame, connection, frame]])
    end

    it "still hands an ordinary frame to the workers" do
      io_manager = build_io_manager
      frame = [13].pack("N") + ("z" * 13)
      connection = connection_sending(frame)

      read_from(io_manager, connection)

      expect(connection).not_to have_received(:close)
      expect(frames_queued(io_manager)).to eq([[:frame, connection, frame]])
    end

    it "drops and closes a connection at the end of its stream" do
      io_manager = build_io_manager
      connection = connection_sending(nil)

      read_from(io_manager, connection)

      expect(connection).to have_received(:close)
      expect(io_manager.instance_variable_get(:@connections)).to be_empty
      expect(frames_queued(io_manager)).to be_empty
    end

    it "lets a worker close a connection while the IO thread is not reading its signals" do
      io_manager = build_io_manager
      connection = double("connection", close: nil)
      signal_pipe = io_manager.instance_variable_get(:@signal_pipes)[1]
      nil until signal_pipe.write_nonblock(" " * 4096, exception: false) == :wait_writable

      Timeout.timeout(2) { io_manager.send(:close_connection, connection) }

      expect(connection).to have_received(:close)
    ensure
      io_manager.send(:close_signal_pipes)
    end

    it "keeps a connection that has nothing to read yet" do
      io_manager = build_io_manager
      connection = connection_sending(:wait_readable)

      read_from(io_manager, connection)

      expect(io_manager.instance_variable_get(:@connections)).to eq([connection])
      expect(frames_queued(io_manager)).to be_empty
    end
  end

  describe "NonblockingServer connection failures" do
    let(:server_transport_class) { Thrift::ServerSocket }

    before(:each) do
      # A "big!" request gets a reply larger than the socket buffers hold, so
      # writing it times out while the client is not reading. A "slow" request
      # gets "pong" after a pause, and a "hold" request once @release lets it
      # go. A "fail" request raises. Anything else gets "pong" at once.
      @write_failures = Queue.new
      @held = Queue.new
      @release = Queue.new
      processor = double("processor")
      allow(processor).to receive(:process) do |iprot, oprot|
        request = iprot.trans.read(4)
        sleep 0.2 if request == "slow"
        if request == "hold"
          @held << true
          @release.pop
        end
        raise "processor failed" if request == "fail"

        reply = request == "big!" ? "x" * (32 * 1024 * 1024) : "pong"
        oprot.trans.write(reply)
        oprot.trans.flush
      rescue Thrift::TransportException => e
        @write_failures << e
        raise
      end

      @log = StringIO.new
      @log_messages = Queue.new
      logger = Logger.new(@log)
      formatter = Logger::Formatter.new
      logger.formatter = proc do |severity, time, progname, message|
        @log_messages << [severity, message]
        formatter.call(severity, time, progname, message)
      end
      @transport = server_transport_class.new("127.0.0.1", 0, client_timeout: 0.2)
      @server = Thrift::NonblockingServer.new(
        processor, @transport, Thrift::FramedTransportFactory.new, nil, 2, logger,
      )
      @server_thread = Thread.new do
        Thread.current.report_on_exception = false
        @server.serve
      end
      wait_until_listening(@transport, @server_thread)
      @port = @transport.handle.addr[1]
      @clients = []
    end

    after(:each) do
      @clients.each(&:close)
      @server.shutdown(1, false) if @server_thread.alive?
      begin
        @server_thread.join(2)
      rescue RuntimeError
        # the example checks how serve ended
      end
      @server_thread.kill if @server_thread.alive?
      @transport.close
    end

    def connect
      client = TCPSocket.new("127.0.0.1", @port)
      @clients << client
      client
    end

    def frame(payload)
      [payload.bytesize].pack("N") + payload
    end

    it "keeps serving other connections after a reply cannot be written" do
      connect.write(frame("big!")) # and never read the reply
      failure = Timeout.timeout(5) { @write_failures.pop }
      expect(failure.type).to eq(Thrift::TransportException::TIMED_OUT)
      # logged as before, not as a frame that could not be decoded
      Timeout.timeout(2) { sleep 0.01 until @log.string.include?("raised error") }
      expect(@log.string).to include("ERROR").and include(__FILE__)
      expect(@log.string).not_to include("could not be decoded")

      client = connect
      client.write(frame("ping"))
      expect(Timeout.timeout(2) { client.read(8) }).to eq(frame("pong"))
      expect(io_thread).to be_alive
      expect(@server_thread).to be_alive
    end

    def logged_messages
      messages = []
      messages << @log_messages.pop until @log_messages.empty?
      messages
    end

    it "closes a connection after a frame that cannot be decoded and logs it once" do
      client = connect
      client.write(frame("")) # a zero-length frame holds no message

      expect(Timeout.timeout(2) { client.read }).to eq("") # the server closed the connection
      Timeout.timeout(2) { sleep 0.01 until @log.string.include?("TransportException") }
      about_it = logged_messages.select { |_, message| message.include?("TransportException") }
      expect(about_it.size).to eq(1)
      severity, message = about_it.first
      expect(severity).to eq("DEBUG")
      expect(message).not_to include("\n")

      other = connect
      other.write(frame("ping"))
      expect(Timeout.timeout(2) { other.read(8) }).to eq(frame("pong"))
    end

    it "drops the requests still queued for a connection it closed after a frame that cannot be decoded" do
      connect.write(frame("hold")) # one worker is left, taking the frames below in order
      Timeout.timeout(2) { @held.pop }
      client = connect
      client.write(frame("") + frame("ping"))

      expect(Timeout.timeout(2) { client.read }).to eq("") # no reply to the ping
      in_progress = io_manager.instance_variable_get(:@frames_in_progress)
      Timeout.timeout(2) { sleep 0.01 until in_progress.size == 1 } # only the held request is left
      expect(@log.string).not_to include("raised error") # the ping was not processed
    ensure
      @release << true
    end

    it "still logs a processor failure with its backtrace and keeps the connection" do
      client = connect
      client.write(frame("fail"))
      Timeout.timeout(2) { sleep 0.01 until @log.string.include?("processor failed") }
      errors = logged_messages.select { |severity, _| severity == "ERROR" }
      expect(errors.size).to eq(1)
      expect(errors.first[1]).to include("processor failed").and include(__FILE__)

      client.write(frame("ping"))
      expect(Timeout.timeout(2) { client.read(8) }).to eq(frame("pong"))
    end

    it "keeps serving other connections after a worker closes a connection select is waiting on" do
      # A worker closes the connection when writing its reply fails. If the
      # close lands while select waits on it, select can raise EBADF rather
      # than IOError; the window is too narrow to hit reliably, so the first
      # select that sees a connection closes it and raises the same way.
      Timeout.timeout(2) { sleep 0.01 until @server.instance_variable_get(:@io_manager) }
      closed = Queue.new
      raised = false
      allow(@server.instance_variable_get(:@io_manager)).to receive(:select).and_wrap_original do |select, readers, *rest|
        connection = readers[1]
        if connection && !raised
          raised = true
          connection.close
          closed << connection
          raise Errno::EBADF
        end
        select.call(readers, *rest)
      end

      connect
      expect(Timeout.timeout(2) { closed.pop }).not_to be_open

      client = connect
      client.write(frame("ping"))
      expect(Timeout.timeout(2) { client.read(8) }).to eq(frame("pong"))
      expect(io_thread).to be_alive
      expect(@server_thread).to be_alive
    end

    it "closes its end of a connection once the client's stream ends and the replies are written" do
      client = connect
      client.write(frame("slow"))
      client.close_write # the stream ends while the request is still being processed

      reply_and_end = Timeout.timeout(2) { client.read } # reads until the server closes
      expect(reply_and_end).to eq(frame("pong"))
    end

    it "closes a connection whose stream has ended once shutdown discards its requests" do
      2.times { connect.write(frame("hold")) } # keeps both workers busy
      2.times { Timeout.timeout(2) { @held.pop } }
      client = connect
      client.write(frame("ping"))
      client.close_write
      ended = io_manager.instance_variable_get(:@ended_connections)
      Timeout.timeout(2) { sleep 0.01 while ended.empty? } # its request is queued

      @server.shutdown(0, false) # waits for the held requests, and discards the queued one
      expect(Timeout.timeout(2) { client.read }).to eq("")
    ensure
      2.times { @release << true }
    end

    it "still closes those connections when a timeout interrupts shutdown as it discards their requests" do
      2.times { connect.write(frame("hold")) } # keeps both workers busy
      2.times { Timeout.timeout(2) { @held.pop } }
      client = connect
      client.write(frame("ping"))
      client.close_write
      ended = io_manager.instance_variable_get(:@ended_connections)
      Timeout.timeout(2) { sleep 0.01 while ended.empty? } # its request is queued

      # Interrupts shutdown from another thread, as Timeout does, once it has
      # taken the queued requests and before it has closed their connections.
      shutdown_thread = Thread.current
      allow(io_manager).to receive(:take_queued_frames).and_wrap_original do |take|
        take.call.tap do
          next unless Thread.current == shutdown_thread # the IO thread takes them too

          Thread.new { shutdown_thread.raise Timeout::Error, "interrupted" }.join
          sleep 0.1
        end
      end
      expect { @server.shutdown }.to raise_error(Timeout::Error, "interrupted")
      expect(Timeout.timeout(2) { client.read }).to eq("")
    ensure
      2.times { @release << true }
    end

    context "when the IO thread fails" do
      let(:server_transport_class) do
        Class.new(Thrift::ServerSocket) do
          def accept
            super.tap do |socket|
              socket.define_singleton_method(:to_io) { raise "unexpected failure" }
            end
          end
        end
      end

      it "logs the failure and stops serving" do
        connect

        expect { @server_thread.join(2) }.to raise_error(RuntimeError, "unexpected failure")
        expect(@log.string).to include("unexpected failure")
        expect(@transport).to be_closed
      end
    end
  end

  describe "#{Thrift::NonblockingServer} with TLS transport" do
    let(:client_timeout) { Thrift::BaseServerTransport::DEFAULT_CLIENT_TIMEOUT }

    before(:each) do
      @port = available_port
      handler = Handler.new
      processor = SpecNamespace::NonblockingService::Processor.new(handler)
      @transport = Thrift::SSLServerSocket.new(
        "localhost",
        @port,
        create_server_ssl_context,
        client_timeout: client_timeout,
      )
      transport_factory = Thrift::FramedTransportFactory.new
      logger = Logger.new(STDERR)
      logger.level = Logger::WARN
      @server = Thrift::NonblockingServer.new(processor, @transport, transport_factory, nil, 5, logger)
      handler.server = @server

      @server_thread = Thread.new(Thread.current) do |master_thread|
        @server.serve
      rescue => e
        master_thread.raise e
      end

      @clients = []
      wait_until_listening(@transport, @server_thread)
    end

    after(:each) do
      @clients.each(&:close)
      @server.shutdown if @server && @server_thread&.alive?
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

    context "when a TLS handshake times out" do
      let(:client_timeout) { 0.1 }

      it "continues accepting connections" do
        stalled_client = TCPSocket.new("localhost", @port)

        expect(Timeout.timeout(1) { stalled_client.read(1) }).to be_nil
        expect(@server_thread).to be_alive

        client = setup_tls_client
        expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      ensure
        stalled_client&.close
      end
    end

    context "when a client resets the connection during the TLS handshake" do
      it "continues accepting connections" do
        reset_client = TCPSocket.new("localhost", @port)
        reset_client.write("\x16\x03\x01\x02\x00\x01".b) # start of a ClientHello record
        reset_client.setsockopt(Socket::SOL_SOCKET, Socket::SO_LINGER, [1, 0].pack("ii"))
        reset_client.close # with a zero linger time, close sends a TCP RST

        client = setup_tls_client
        expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
        expect(@server_thread).to be_alive
      ensure
        reset_client&.close
      end
    end

    it "keeps answering other clients while one client has sent part of a TLS record" do
      stalled_socket = Thrift::SSLSocket.new("localhost", @port, nil, create_client_ssl_context)
      stalled_client = setup_tls_client(stalled_socket)
      expect(stalled_client.greeting(true)).to eq(SpecNamespace::Hello.new)
      client = setup_tls_client

      stalled_io = stalled_socket.to_io
      stalled_io.write("\x17\x03\x03".b) # the start of an application data record header
      reply = Timeout.timeout(2) { client.greeting(true) }
      expect(reply).to eq(SpecNamespace::Hello.new)
    ensure
      # Reset the connection so the unfinished record ends with ECONNRESET, not an SSL error.
      stalled_io&.setsockopt(Socket::SOL_SOCKET, Socket::SO_LINGER, [1, 0].pack("ii"))
      stalled_io&.close
    end

    it "reads a request that arrives in several TLS records, each larger than one read" do
      # What is left of a record after one read is buffered by OpenSSL, where
      # select cannot see it, so the server has to read it without waiting.
      stub_const("Thrift::NonblockingServer::IOManager::DEFAULT_BUFFER", 4)
      socket = Thrift::SSLSocket.new("localhost", @port, nil, create_client_ssl_context)
      records = []
      allow(socket).to receive(:write).and_wrap_original do |write, data|
        data.b.scan(/.{1,10}/m).each do |record| # one TLS record per write
          records << record
          write.call(record)
        end
      end
      client = setup_tls_client(socket)

      reply = Timeout.timeout(2) { client.greeting(true) }
      expect(reply).to eq(SpecNamespace::Hello.new)
      expect(records.length).to be > 2
    end

    context "when a connection's stream ends inside a TLS record" do
      it "keeps serving other connections" do
        broken = Thrift::SSLSocket.new("localhost", @port, nil, create_client_ssl_context)
        broken.open
        broken.to_io.write("\x17\x03\x03".b) # the start of a record header
        broken.close

        client = setup_tls_client
        expect(Timeout.timeout(2) { client.greeting(true) }).to eq(SpecNamespace::Hello.new)
        expect(io_thread).to be_alive
      end
    end

    def setup_tls_client(socket = Thrift::SSLSocket.new("localhost", @port, nil, create_client_ssl_context))
      transport = Thrift::FramedTransport.new(socket)
      protocol = Thrift::BinaryProtocol.new(transport)
      client = SpecNamespace::NonblockingService::Client.new(protocol)
      transport.open
      @clients << transport
      client
    end

    def ssl_keys_dir
      File.expand_path("../../../test/keys", __dir__)
    end

    def create_server_ssl_context
      OpenSSL::SSL::SSLContext.new.tap do |ctx|
        ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
        if ctx.respond_to?(:min_version=) && OpenSSL::SSL.const_defined?(:TLS1_2_VERSION)
          ctx.min_version = OpenSSL::SSL::TLS1_2_VERSION
        end
        ctx.ca_file = File.join(ssl_keys_dir, "CA.pem")
        ctx.cert = OpenSSL::X509::Certificate.new(File.read(File.join(ssl_keys_dir, "server.crt")))
        ctx.cert_store = OpenSSL::X509::Store.new
        ctx.cert_store.add_file(File.join(ssl_keys_dir, "client.pem"))
        ctx.key = OpenSSL::PKey::RSA.new(File.read(File.join(ssl_keys_dir, "server.key")))
      end
    end

    def create_client_ssl_context
      OpenSSL::SSL::SSLContext.new.tap do |ctx|
        ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
        if ctx.respond_to?(:min_version=) && OpenSSL::SSL.const_defined?(:TLS1_2_VERSION)
          ctx.min_version = OpenSSL::SSL::TLS1_2_VERSION
        end
        ctx.ca_file = File.join(ssl_keys_dir, "CA.pem")
        ctx.cert = OpenSSL::X509::Certificate.new(File.read(File.join(ssl_keys_dir, "client.crt")))
        ctx.cert_store = OpenSSL::X509::Store.new
        ctx.cert_store.add_file(File.join(ssl_keys_dir, "server.pem"))
        ctx.key = OpenSSL::PKey::RSA.new(File.read(File.join(ssl_keys_dir, "client.key")))
      end
    end
  end

  describe "NonblockingServer workers" do
    # Raises a ScriptError, outside StandardError, for the first frame it is
    # handed, and hands every later frame to the wrapped processor.
    class NotImplementedOnceFrameProcessor
      attr_reader :raised

      def initialize(inner)
        @inner = inner
        @raised = Queue.new
        @done = false
      end

      def process(iprot, oprot)
        unless @done
          @done = true
          @raised << true
          raise NotImplementedError, "processor not implemented"
        end
        @inner.process(iprot, oprot)
      end
    end

    it "keep serving after the processor raises outside StandardError" do
      port = available_port
      queue = Queue.new
      transport = SpecServerSocket.new("localhost", port, queue)
      processor = NotImplementedOnceFrameProcessor.new(SpecNamespace::NonblockingService::Processor.new(Handler.new))
      server = Thrift::NonblockingServer.new(
        processor, transport, Thrift::FramedTransportFactory.new, nil, 1, Logger.new(nil),
      )
      server_thread = Thread.new { server.serve }
      queue.pop
      wait_until_listening(transport, server_thread)

      # The server's one worker is handed this frame first.
      failing_transport = Thrift::FramedTransport.new(Thrift::Socket.new("localhost", port, 5))
      failing_transport.open
      SpecNamespace::NonblockingService::Client.new(Thrift::BinaryProtocol.new(failing_transport)).send_greeting(true)
      Timeout.timeout(5) { processor.raised.pop }

      client_transport = Thrift::FramedTransport.new(Thrift::Socket.new("localhost", port, 5))
      client = SpecNamespace::NonblockingService::Client.new(Thrift::BinaryProtocol.new(client_transport))
      client_transport.open
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
    ensure
      failing_transport&.close
      client_transport&.close
      server&.shutdown(1, false)
      server_thread&.join(2)
      server_thread&.kill if server_thread&.alive?
      transport&.close
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

  def io_manager
    @server.instance_variable_get(:@io_manager)
  end

  def io_thread
    io_manager.instance_variable_get(:@iom_thread)
  end

  def available_port
    TCPServer.open("localhost", 0) { |server| server.addr[1] }
  end
end
