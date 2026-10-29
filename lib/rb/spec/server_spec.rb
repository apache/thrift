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
require "openssl"
require "stringio"
require "timeout"

describe "Server" do
  describe Thrift::BaseServer do
    before(:each) do
      @processor = double("Processor")
      @serverTrans = double("ServerTransport")
      @trans = double("BaseTransport")
      @prot = double("BaseProtocol")
      @server = described_class.new(@processor, @serverTrans, @trans, @prot)
    end

    it "should default to BaseTransportFactory and BinaryProtocolFactory when not specified" do
      @server = Thrift::BaseServer.new(double("Processor"), double("BaseServerTransport"))
      expect(@server.instance_variable_get(:'@transport_factory')).to be_an_instance_of(Thrift::BaseTransportFactory)
      expect(@server.instance_variable_get(:'@protocol_factory')).to be_an_instance_of(Thrift::BinaryProtocolFactory)
    end

    it "should not serve" do
      expect { @server.serve }.to raise_error(NotImplementedError)
    end

    it "should provide a reasonable to_s" do
      expect(@serverTrans).to receive(:to_s).once.and_return("serverTrans")
      expect(@trans).to receive(:to_s).once.and_return("trans")
      expect(@prot).to receive(:to_s).once.and_return("prot")
      expect(@server.to_s).to eq("server(prot(trans(serverTrans)))")
    end
  end

  describe Thrift::SimpleServer do
    class EphemeralServerSocket < Thrift::ServerSocket
      def initialize(ready)
        super("127.0.0.1", 0)
        @ready = ready
      end

      def listen
        super
        @ready << handle.addr[1]
      end
    end

    class StopAfterVoidHandler
      attr_reader :calls

      def initialize
        @calls = 0
      end

      def voidMethod
        @calls += 1
        throw :stop
      end
    end

    before(:each) do
      @processor = double("Processor")
      @serverTrans = double("ServerTransport")
      @trans = double("BaseTransport")
      @prot = double("BaseProtocol")
      @client = double("Client")
      @server = described_class.new(@processor, @serverTrans, @trans, @prot)
    end

    it "should provide a reasonable to_s" do
      expect(@serverTrans).to receive(:to_s).once.and_return("serverTrans")
      expect(@trans).to receive(:to_s).once.and_return("trans")
      expect(@prot).to receive(:to_s).once.and_return("prot")
      expect(@server.to_s).to eq("simple(server(prot(trans(serverTrans))))")
    end

    it "should serve in the main thread" do
      expect(@serverTrans).to receive(:listen).ordered
      expect(@serverTrans).to receive(:accept).exactly(3).times.and_return(@client)
      expect(@trans).to receive(:get_transport).exactly(3).times.with(@client).and_return(@trans)
      expect(@prot).to receive(:get_protocol).exactly(3).times.with(@trans).and_return(@prot)
      x = 0
      expect(@processor).to receive(:process).exactly(3).times.with(@prot, @prot) do
        case (x += 1)
        when 1 then raise Thrift::TransportException
        when 2 then raise Thrift::ProtocolException
        when 3 then throw :stop
        end
      end
      expect(@trans).to receive(:close).exactly(3).times
      expect(@serverTrans).to receive(:close).ordered
      expect { @server.serve }.to throw_symbol(:stop)
    end

    it "should continue serving after accept raises Errno::ECONNRESET" do
      expect(@serverTrans).to receive(:listen).ordered
      expect(@serverTrans).to receive(:accept).ordered.and_raise(Errno::ECONNRESET)
      expect(@serverTrans).to receive(:accept).ordered.and_return(@client)
      expect(@trans).to receive(:get_transport).once.with(@client).and_return(@trans)
      expect(@prot).to receive(:get_protocol).once.with(@trans).and_return(@prot)
      expect(@processor).to receive(:process).once.with(@prot, @prot) { throw :stop }
      expect(@trans).to receive(:close).once
      expect(@serverTrans).to receive(:close).ordered
      expect { @server.serve }.to throw_symbol(:stop)
    end

    it "should continue serving after accept raises OpenSSL::SSL::SSLError" do
      expect(@serverTrans).to receive(:listen).ordered
      expect(@serverTrans).to receive(:accept).ordered.and_raise(OpenSSL::SSL::SSLError)
      expect(@serverTrans).to receive(:accept).ordered.and_return(@client)
      expect(@trans).to receive(:get_transport).once.with(@client).and_return(@trans)
      expect(@prot).to receive(:get_protocol).once.with(@trans).and_return(@prot)
      expect(@processor).to receive(:process).once.with(@prot, @prot) { throw :stop }
      expect(@trans).to receive(:close).once
      expect(@serverTrans).to receive(:close).ordered
      expect { @server.serve }.to throw_symbol(:stop)
    end

    [
      [
        "compact unknown type", Thrift::CompactProtocolFactory.new, proc do
          trans = Thrift::MemoryBufferTransport.new
          prot = Thrift::CompactProtocol.new(trans)
          prot.write_message_begin("unknown", Thrift::MessageTypes::CALL, 1)
          trans.write([0x1e, 0].pack("C*"))
          trans.read(trans.available)
        end,
      ],
      ["JSON unknown type", Thrift::JsonProtocolFactory.new, proc { '[1,"unknown",1,1,{"1":{"wat":0}}]' }],
      ["short JSON UUID", Thrift::JsonProtocolFactory.new, proc { '[1,"unknown",1,1,{"1":{"uid":"x"}}]' }],
    ].each do |failure_type, protocol_factory, malformed_request|
      it "closes a malformed #{failure_type} connection and continues accepting clients" do
        ready = Queue.new
        errors = Queue.new
        server_transport = EphemeralServerSocket.new(ready)
        handler = StopAfterVoidHandler.new
        processor = Thrift::Test::Srv::Processor.new(handler)
        server = Thrift::SimpleServer.new(processor, server_transport, nil, protocol_factory)
        server_thread = Thread.new do
          catch(:stop) { server.serve }
        rescue StandardError, ScriptError => error
          errors << error
        end
        server_thread.report_on_exception = false

        port = Timeout.timeout(2) { ready.pop }
        malformed_client = TCPSocket.new("127.0.0.1", port)
        malformed_client.write(malformed_request.call)
        malformed_client.close_write

        expect(IO.select([malformed_client], nil, nil, 2)).not_to be_nil
        peer_closed = begin
          malformed_client.readpartial(1)
          false
        rescue EOFError, Errno::ECONNRESET
          true
        end
        expect(peer_closed).to be(true)
        expect(server_thread).to be_alive

        valid_transport = Thrift::Socket.new("127.0.0.1", port)
        valid_transport.open
        valid_protocol = protocol_factory.get_protocol(valid_transport)
        Thrift::Test::Srv::Client.new(valid_protocol).send_voidMethod

        expect(server_thread.join(2)).to eq(server_thread)
        expect(handler.calls).to eq(1)
        expect(errors).to be_empty
      ensure
        malformed_client&.close
        valid_transport&.close
        server_transport&.close
        server_thread&.kill
        server_thread&.join
      end
    end

    [
      [
        "binary", Thrift::BinaryProtocolFactory.new, proc do
          method_name = [0xff, 0xfe].pack("C*")
          [0x80010000 | Thrift::MessageTypes::CALL].pack("N") +
            [method_name.bytesize].pack("N") + method_name + [1].pack("N") + [Thrift::Types::STOP].pack("C")
        end,
      ],
      [
        "compact", Thrift::CompactProtocolFactory.new, proc do
          method_name = [0xff, 0xfe].pack("C*")
          [0x82, 0x21, 0x01, method_name.bytesize].pack("C*") + method_name + [Thrift::Types::STOP].pack("C")
        end,
      ],
    ].each do |protocol_name, protocol_factory, invalid_request|
      it "answers a #{protocol_name} method name that is not valid UTF-8 with UNKNOWN_METHOD and continues accepting clients" do
        ready = Queue.new
        errors = Queue.new
        server_transport = EphemeralServerSocket.new(ready)
        handler = StopAfterVoidHandler.new
        processor = Thrift::Test::Srv::Processor.new(handler)
        server = Thrift::SimpleServer.new(processor, server_transport, nil, protocol_factory)
        server_thread = Thread.new do
          catch(:stop) { server.serve }
        rescue StandardError, ScriptError => error
          errors << error
        end
        server_thread.report_on_exception = false

        port = Timeout.timeout(2) { ready.pop }
        invalid_client = TCPSocket.new("127.0.0.1", port)
        invalid_client.write(invalid_request.call)
        invalid_client.close_write

        reply_bytes = begin
          Timeout.timeout(2) { invalid_client.read }
        rescue Errno::ECONNRESET
          ""
        end
        expect(reply_bytes).not_to be_empty
        reply = protocol_factory.get_protocol(Thrift::MemoryBufferTransport.new(reply_bytes))
        _name, message_type, seqid = reply.read_message_begin
        exception = Thrift::ApplicationException.new
        exception.read(reply)
        reply.read_message_end
        expect(message_type).to eq(Thrift::MessageTypes::EXCEPTION)
        expect(seqid).to eq(1)
        expect(exception.type).to eq(Thrift::ApplicationException::UNKNOWN_METHOD)
        expect(server_thread).to be_alive

        valid_transport = Thrift::Socket.new("127.0.0.1", port)
        valid_transport.open
        valid_protocol = protocol_factory.get_protocol(valid_transport)
        Thrift::Test::Srv::Client.new(valid_protocol).send_voidMethod

        expect(server_thread.join(2)).to eq(server_thread)
        expect(handler.calls).to eq(1)
        expect(errors).to be_empty
      ensure
        invalid_client&.close
        valid_transport&.close
        server_transport&.close
        server_thread&.kill
        server_thread&.join
      end
    end

    it "closes a zero-length framed connection and continues accepting clients" do
      ready = Queue.new
      errors = Queue.new
      server_transport = EphemeralServerSocket.new(ready)
      handler = StopAfterVoidHandler.new
      processor = Thrift::Test::Srv::Processor.new(handler)
      transport_factory = Thrift::FramedTransportFactory.new
      protocol_factory = Thrift::CompactProtocolFactory.new
      server = Thrift::SimpleServer.new(processor, server_transport, transport_factory, protocol_factory)
      server_thread = Thread.new do
        catch(:stop) { server.serve }
      rescue StandardError, ScriptError => error
        errors << error
      end
      server_thread.report_on_exception = false

      port = Timeout.timeout(2) { ready.pop }
      malformed_client = TCPSocket.new("127.0.0.1", port)
      malformed_client.write([0].pack("N"))
      malformed_client.close_write

      expect(IO.select([malformed_client], nil, nil, 2)).not_to be_nil
      expect(malformed_client.read).to eq("")
      expect(server_thread).to be_alive

      socket = Thrift::Socket.new("127.0.0.1", port)
      valid_transport = Thrift::FramedTransport.new(socket)
      valid_transport.open
      valid_protocol = Thrift::CompactProtocol.new(valid_transport)
      Thrift::Test::Srv::Client.new(valid_protocol).send_voidMethod

      expect(server_thread.join(2)).to eq(server_thread)
      expect(handler.calls).to eq(1)
      expect(errors).to be_empty
    ensure
      malformed_client&.close
      valid_transport&.close
      server_transport&.close
      server_thread&.kill
      server_thread&.join
    end

    it "closes a connection whose multiplexed dispatch fails and continues accepting clients" do
      ready = Queue.new
      errors = Queue.new
      server_transport = EphemeralServerSocket.new(ready)
      handler = StopAfterVoidHandler.new
      protocol_factory = Thrift::BinaryProtocolFactory.new
      processor = Thrift::MultiplexedProcessor.new
      processor.register_processor("Srv", Thrift::Test::Srv::Processor.new(handler))
      server = Thrift::SimpleServer.new(processor, server_transport, nil, protocol_factory)
      server_thread = Thread.new do
        catch(:stop) { server.serve }
      rescue StandardError, ScriptError => error
        errors << error
      end
      server_thread.report_on_exception = false

      # A call without a "service:" prefix matches no registered processor.
      unprefixed_request = Thrift::MemoryBufferTransport.new
      Thrift::Test::Srv::Client.new(protocol_factory.get_protocol(unprefixed_request)).send_voidMethod

      port = Timeout.timeout(2) { ready.pop }
      malformed_client = TCPSocket.new("127.0.0.1", port)
      malformed_client.write(unprefixed_request.read(unprefixed_request.available))
      malformed_client.close_write

      expect(IO.select([malformed_client], nil, nil, 2)).not_to be_nil
      peer_closed = begin
        malformed_client.readpartial(1)
        false
      rescue EOFError, Errno::ECONNRESET
        true
      end
      expect(peer_closed).to be(true)
      expect(server_thread).to be_alive

      valid_transport = Thrift::Socket.new("127.0.0.1", port)
      valid_transport.open
      valid_protocol = Thrift::MultiplexedProtocol.new(protocol_factory.get_protocol(valid_transport), "Srv")
      Thrift::Test::Srv::Client.new(valid_protocol).send_voidMethod

      expect(server_thread.join(2)).to eq(server_thread)
      expect(handler.calls).to eq(1)
      expect(errors).to be_empty
    ensure
      malformed_client&.close
      valid_transport&.close
      server_transport&.close
      server_thread&.kill
      server_thread&.join
    end
  end

  describe Thrift::ThreadedServer do
    before(:each) do
      # These examples run stubbed thread blocks on the example thread.
      allow(Thread.current).to receive(:name=)
      @processor = double("Processor")
      @serverTrans = double("ServerTransport")
      @trans = double("BaseTransport")
      @prot = double("BaseProtocol")
      @client = double("Client")
      @server = described_class.new(@processor, @serverTrans, @trans, @prot)
    end

    it "should provide a reasonable to_s" do
      expect(@serverTrans).to receive(:to_s).once.and_return("serverTrans")
      expect(@trans).to receive(:to_s).once.and_return("trans")
      expect(@prot).to receive(:to_s).once.and_return("prot")
      expect(@server.to_s).to eq("threaded(server(prot(trans(serverTrans))))")
    end

    it "should serve using threads" do
      expect(@serverTrans).to receive(:listen).ordered
      expect(@serverTrans).to receive(:accept).exactly(3).times.and_return(@client)
      expect(@trans).to receive(:get_transport).exactly(3).times.with(@client).and_return(@trans)
      expect(@prot).to receive(:get_protocol).exactly(3).times.with(@trans).and_return(@prot)
      expect(Thread).to receive(:new).with(@prot, @trans).exactly(3).times.and_yield(@prot, @trans)
      x = 0
      expect(@processor).to receive(:process).exactly(3).times.with(@prot, @prot) do
        case (x += 1)
        when 1 then raise Thrift::TransportException
        when 2 then raise Thrift::ProtocolException
        when 3 then throw :stop
        end
      end
      expect(@trans).to receive(:close).exactly(3).times
      expect(@serverTrans).to receive(:close).ordered
      expect { @server.serve }.to throw_symbol(:stop)
    end

    it "should continue serving after accept raises Errno::ECONNRESET" do
      expect(@serverTrans).to receive(:listen).ordered
      expect(@serverTrans).to receive(:accept).ordered.and_raise(Errno::ECONNRESET)
      expect(@serverTrans).to receive(:accept).ordered.and_return(@client)
      expect(@trans).to receive(:get_transport).once.with(@client).and_return(@trans)
      expect(@prot).to receive(:get_protocol).once.with(@trans).and_return(@prot)
      expect(Thread).to receive(:new).with(@prot, @trans).once.and_yield(@prot, @trans)
      expect(@processor).to receive(:process).once.with(@prot, @prot) { throw :stop }
      expect(@trans).to receive(:close).once
      expect(@serverTrans).to receive(:close).ordered
      expect { @server.serve }.to throw_symbol(:stop)
    end

    it "should continue serving after accept raises OpenSSL::SSL::SSLError" do
      expect(@serverTrans).to receive(:listen).ordered
      expect(@serverTrans).to receive(:accept).ordered.and_raise(OpenSSL::SSL::SSLError)
      expect(@serverTrans).to receive(:accept).ordered.and_return(@client)
      expect(@trans).to receive(:get_transport).once.with(@client).and_return(@trans)
      expect(@prot).to receive(:get_protocol).once.with(@trans).and_return(@prot)
      expect(Thread).to receive(:new).with(@prot, @trans).once.and_yield(@prot, @trans)
      expect(@processor).to receive(:process).once.with(@prot, @prot) { throw :stop }
      expect(@trans).to receive(:close).once
      expect(@serverTrans).to receive(:close).ordered
      expect { @server.serve }.to throw_symbol(:stop)
    end
  end

  describe Thrift::ThreadPoolServer do
    before(:each) do
      # These examples run stubbed thread blocks on the example thread.
      allow(Thread.current).to receive(:name=)
      @processor = double("Processor")
      @server_trans = double("ServerTransport")
      @trans = double("BaseTransport")
      @prot = double("BaseProtocol")
      @client = double("Client")
      @server = described_class.new(@processor, @server_trans, @trans, @prot)
      sleep(0.15)
    end

    it "should provide a reasonable to_s" do
      expect(@server_trans).to receive(:to_s).once.and_return("server_trans")
      expect(@trans).to receive(:to_s).once.and_return("trans")
      expect(@prot).to receive(:to_s).once.and_return("prot")
      expect(@server.to_s).to eq("threadpool(server(prot(trans(server_trans))))")
    end

    it "should serve inside a thread" do
      exception_q = @server.instance_variable_get(:@exception_q)
      expect_any_instance_of(described_class).to receive(:serve) do
        exception_q.push(StandardError.new("ERROR"))
      end
      expect { @server.rescuable_serve }.to(raise_error("ERROR"))
      sleep(0.15)
    end

    it "should avoid running the server twice when retrying rescuable_serve" do
      exception_q = @server.instance_variable_get(:@exception_q)
      expect_any_instance_of(described_class).to receive(:serve) do
        exception_q.push(StandardError.new("ERROR1"))
        exception_q.push(StandardError.new("ERROR2"))
      end
      expect { @server.rescuable_serve }.to(raise_error("ERROR1"))
      expect { @server.rescuable_serve }.to(raise_error("ERROR2"))
    end

    it "should serve using a thread pool" do
      thread_q = double("SizedQueue")
      exception_q = double("Queue")
      logger = double("Logger")
      @server.instance_variable_set(:@thread_q, thread_q)
      @server.instance_variable_set(:@exception_q, exception_q)
      @server.instance_variable_set(:@logger, logger)
      expect(@server_trans).to receive(:listen).ordered
      expect(thread_q).to receive(:push).with(:token)
      expect(thread_q).to receive(:pop)
      expect(Thread).to receive(:new).with(0).and_yield(0)
      expect(@server_trans).to receive(:accept).exactly(3).times.and_return(@client)
      expect(@trans).to receive(:get_transport).exactly(3).times.and_return(@trans)
      expect(@prot).to receive(:get_protocol).exactly(3).times.and_return(@prot)
      x = 0
      error = RuntimeError.new("Stopped")
      expect(@processor).to receive(:process).exactly(3).times.with(@prot, @prot) do
        case (x += 1)
        when 1 then raise Thrift::TransportException
        when 2 then raise Thrift::ProtocolException
        when 3 then raise error
        end
      end
      expect(@trans).to receive(:close).exactly(3).times
      # Only rescuable_serve reads the exception queue, so serve logs the error instead.
      allow(exception_q).to receive(:push).and_throw(:stop)
      expect(logger).to receive(:error).with("Error while serving a client connection: #{error.inspect}").and_throw(:stop)
      expect(@server_trans).to receive(:close)
      expect { @server.serve }.to(throw_symbol(:stop))
      expect(exception_q).not_to have_received(:push)
    end

    it "should not enqueue TLS accept errors" do
      exception_q = @server.instance_variable_get(:@exception_q)
      expect(@server_trans).to receive(:listen).ordered
      expect(@server_trans).to receive(:accept).ordered.and_raise(OpenSSL::SSL::SSLError)
      expect(@server_trans).to receive(:accept).ordered.and_return(@client)
      expect(@trans).to receive(:get_transport).once.with(@client).and_return(@trans)
      expect(@prot).to receive(:get_protocol).once.with(@trans).and_return(@prot)
      allow(Thread).to receive(:new).with(0).and_yield(0)
      expect(@processor).to receive(:process).once.with(@prot, @prot) { throw :stop }
      expect(@trans).to receive(:close).once
      expect(@server_trans).to receive(:close)

      catch(:stop) { @server.serve }

      expect(exception_q).to be_empty
    end

    it "accepts a logger" do
      logger = Logger.new(StringIO.new)
      server = described_class.new(@processor, @server_trans, @trans, @prot, 1, logger: logger)
      expect(server.instance_variable_get(:@logger)).to equal(logger)
    end

    context "with client connections" do
      before(:each) do
        @threads = ThreadGroup.new
        @ready = Queue.new
        @log = StringIO.new
        @protocol_factory = Thrift::BinaryProtocolFactory.new
        processor = Thrift::MultiplexedProcessor.new
        processor.register_processor("Srv", Thrift::Test::Srv::Processor.new(double("Handler", voidMethod: nil)))
        @server_transport = EphemeralServerSocket.new(@ready)
        # With one worker, a replacement worker starts only after the failed one is done.
        @pool_server = described_class.new(processor, @server_transport, nil, @protocol_factory, 1)
        @pool_server.instance_variable_set(:@logger, Logger.new(@log))
      end

      after(:each) do
        until (threads = @threads.list).empty?
          threads.each(&:kill)
          threads.each(&:join)
        end
        @server_transport.close
      end

      def in_server_threads
        previous_group = Thread.current.group
        # Children inherit the group before their thread blocks can run.
        @threads.add(Thread.current)
        yield
      ensure
        previous_group.add(Thread.current)
      end

      # A call without a "service:" prefix matches no registered processor.
      def send_unprefixed_call(port)
        request = Thrift::MemoryBufferTransport.new
        Thrift::Test::Srv::Client.new(@protocol_factory.get_protocol(request)).send_voidMethod
        client = TCPSocket.new("127.0.0.1", port)
        client.write(request.read(request.available))
        client.close_write

        expect(IO.select([client], nil, nil, 2)).not_to be_nil
        peer_closed = begin
          client.readpartial(1)
          false
        rescue EOFError, Errno::ECONNRESET
          true
        end
        expect(peer_closed).to be(true)
      ensure
        client&.close
      end

      def call_void_method(port)
        transport = Thrift::Socket.new("127.0.0.1", port)
        transport.open
        protocol = Thrift::MultiplexedProtocol.new(@protocol_factory.get_protocol(transport), "Srv")
        Timeout.timeout(2) { Thrift::Test::Srv::Client.new(protocol).voidMethod }
      ensure
        transport&.close
      end

      it "logs a failed connection under serve and keeps serving without keeping the error" do
        server_thread = in_server_threads { Thread.new { @pool_server.serve } }
        port = Timeout.timeout(2) { @ready.pop }

        send_unprefixed_call(port)
        call_void_method(port)

        expect(server_thread).to be_alive
        expect(@pool_server.instance_variable_get(:@exception_q).size).to eq(0)
        expect(@log.string).to include(
          "Error while serving a client connection: #<Thrift::Exception: Service name not found in message name: voidMethod.",
        )
      end

      it "relays a worker exception to rescuable_serve" do
        relayed = Queue.new
        in_server_threads do
          Thread.new do
            @pool_server.rescuable_serve
          rescue StandardError => e
            relayed << e
          end
        end
        port = Timeout.timeout(2) { @ready.pop }

        send_unprefixed_call(port)
        error = Timeout.timeout(2) { relayed.pop }

        expect(error).to be_a(Thrift::Exception)
        expect(error.message).to start_with("Service name not found in message name: voidMethod.")
        call_void_method(port)
        expect(@log.string).to be_empty
      end
    end
  end

  describe "thread names" do
    before(:each) do
      @threads = ThreadGroup.new
    end

    after(:each) do
      stop_threads
    end

    def stop_threads
      @server_thread&.kill
      @server_thread&.join
      until (threads = @threads.list).empty?
        threads.each(&:kill)
        threads.each(&:join)
      end
    end

    def with_server_threads
      previous_group = Thread.current.group
      # Children inherit the group before their thread blocks can run.
      @threads.add(Thread.current)
      yield
    ensure
      previous_group.add(Thread.current)
    end

    def start_server(server)
      with_server_threads do
        @server_thread = Thread.new do
          Thread.current.name = "application"
          server.serve
        end
      end
    end

    def receive_threads(queue, count = 1)
      Timeout.timeout(5) { Array.new(count) { queue.pop } }
    end

    it "stops pool workers that have not reported their names" do
      entered = Queue.new
      transport = double("server transport", listen: nil, close: nil)
      allow(transport).to receive(:accept) do
        entered << Thread.current
        sleep
      end
      server = Thrift::ThreadPoolServer.new(nil, transport, nil, nil, 1)

      start_server(server)
      worker = receive_threads(entered).first
      stop_threads

      expect(worker).not_to be_alive
    ensure
      worker&.kill
      worker&.join
    end

    it "names connection handlers before processing without renaming the serving thread" do
      clients = Queue.new
      clients << Thrift::MemoryBufferTransport.new
      transport = double("server transport", listen: nil, close: nil)
      allow(transport).to receive(:accept) { clients.pop }
      names = Queue.new
      processor = double("processor")
      allow(processor).to receive(:process) do
        names << Thread.current.name
        sleep
      end
      server = Thrift::ThreadedServer.new(processor, transport)

      start_server(server)

      expect(receive_threads(names)).to eq(["thrift.threaded"])
      expect(@server_thread.name).to eq("application")
    end

    it "numbers pool workers by creation order, including replacements" do
      accepting = Queue.new
      transport = double("server transport", listen: nil, close: nil)
      allow(transport).to receive(:accept) do
        commands = Queue.new
        accepting << [Thread.current.name, commands]
        commands.pop
        raise "worker failed"
      end
      server = Thrift::ThreadPoolServer.new(nil, transport, nil, nil, 2)

      start_server(server)
      workers = receive_threads(accepting, 2).sort_by(&:first)

      expect(workers.map(&:first)).to eq(["thrift.pool.0", "thrift.pool.1"])
      workers.first.last << :fail
      expect(receive_threads(accepting).first.first).to eq("thrift.pool.2")
      expect(@server_thread.name).to eq("application")
    end

    it "restarts pool worker numbering for each server run" do
      names = Queue.new
      transport = double("server transport", listen: nil, close: nil)
      allow(transport).to receive(:accept) do
        names << Thread.current.name
        sleep
      end
      server = Thrift::ThreadPoolServer.new(nil, transport, nil, nil, 1)

      2.times do
        start_server(server)
        expect(receive_threads(names)).to eq(["thrift.pool.0"])
        stop_threads
      end
    end

    it "names the background server before entering serve" do
      names = Queue.new
      server = Thrift::ThreadPoolServer.new(nil, nil)
      allow(server).to receive(:serve) do
        names << Thread.current.name
        server.instance_variable_get(:@exception_q) << RuntimeError.new("stopped")
      end
      caller_name = Thread.current.name

      expect do
        Timeout.timeout(5) { with_server_threads { server.rescuable_serve } }
      end.to raise_error("stopped")
      expect(receive_threads(names)).to eq(["thrift.server"])
      expect(Thread.current.name).to eq(caller_name)
    end

    it "names nonblocking I/O and indexed workers before their first log messages" do
      names = Queue.new
      logger = double("logger")
      allow(logger).to receive(:debug) do |message|
        names << Thread.current.name if message.start_with?("Starting ") || message.end_with?("is spawning")
      end
      io_manager = Thrift::NonblockingServer::IOManager.new(nil, nil, nil, nil, 12, logger)
      caller_name = Thread.current.name

      with_server_threads { io_manager.spawn }

      expect(receive_threads(names, 13)).to match_array(["thrift.io", *(0...12).map { |i| "thrift.nb.#{i}" }])
      expect(Thread.current.name).to eq(caller_name)
    ensure
      io_manager&.ensure_closed
    end

    [true, false].each do |blocking|
      it "#{blocking ? "preserves the caller name during synchronous" : "names the asynchronous"} shutdown" do
        names = Queue.new
        io_manager = double("I/O manager")
        allow(io_manager).to receive(:shutdown) { names << Thread.current.name }
        transport = double("server transport", close: nil)
        server = Thrift::NonblockingServer.new(nil, transport)
        server.instance_variable_set(:@io_manager, io_manager)
        caller_name = Thread.current.name

        thread = with_server_threads { server.shutdown(0, blocking) }
        thread.join unless blocking

        expect(receive_threads(names)).to eq([blocking ? caller_name : "thrift.shutdown"])
        expect(Thread.current.name).to eq(caller_name)
      end
    end
  end
end
