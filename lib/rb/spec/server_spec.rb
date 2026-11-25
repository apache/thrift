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
require "json"
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

    # Reads the first call it is handed and raises a ScriptError, outside
    # StandardError, for it; hands every later call to the wrapped processor.
    class NotImplementedOnceProcessor
      def initialize(inner)
        @inner = inner
        @raised = false
      end

      def process(iprot, oprot)
        unless @raised
          @raised = true
          iprot.read_message_begin
          iprot.skip(Thrift::Types::STRUCT)
          iprot.read_message_end
          raise NotImplementedError, "processor not implemented"
        end
        @inner.process(iprot, oprot)
      end
    end

    it "closes a connection whose processor raises outside StandardError and continues accepting clients" do
      ready = Queue.new
      errors = Queue.new
      server_transport = EphemeralServerSocket.new(ready)
      handler = StopAfterVoidHandler.new
      protocol_factory = Thrift::BinaryProtocolFactory.new
      processor = NotImplementedOnceProcessor.new(Thrift::Test::Srv::Processor.new(handler))
      server = Thrift::SimpleServer.new(processor, server_transport, nil, protocol_factory)
      server_thread = Thread.new do
        catch(:stop) { server.serve }
      rescue StandardError, ScriptError, SystemStackError => error
        errors << error
      end
      server_thread.report_on_exception = false

      port = Timeout.timeout(2) { ready.pop }
      failing_transport = Thrift::Socket.new("127.0.0.1", port)
      failing_transport.open
      Thrift::Test::Srv::Client.new(protocol_factory.get_protocol(failing_transport)).send_voidMethod

      valid_transport = Thrift::Socket.new("127.0.0.1", port)
      valid_transport.open
      Thrift::Test::Srv::Client.new(protocol_factory.get_protocol(valid_transport)).send_voidMethod

      expect(server_thread.join(2)).to eq(server_thread)
      expect(errors).to be_empty
      expect(handler.calls).to eq(1)
    ensure
      failing_transport&.close
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

    [OpenSSL::SSL::SSLError, Errno::ECONNRESET, Errno::EPIPE].each do |error_class|
      it "should keep the worker accepting after accept raises #{error_class}" do
        exception_q = @server.instance_variable_get(:@exception_q)
        expect(@server_trans).to receive(:listen).ordered
        expect(@server_trans).to receive(:accept).ordered.and_raise(error_class)
        expect(@server_trans).to receive(:accept).ordered.and_return(@client)
        expect(@trans).to receive(:get_transport).once.with(@client).and_return(@trans)
        expect(@prot).to receive(:get_protocol).once.with(@trans).and_return(@prot)
        # The first worker accepts the next client; no replacement is spawned.
        expect(Thread).to receive(:new).with(0).once.and_yield(0)
        expect(@processor).to receive(:process).once.with(@prot, @prot) { throw :stop }
        expect(@trans).to receive(:close).once
        expect(@server_trans).to receive(:close)

        catch(:stop) { @server.serve }

        expect(exception_q).to be_empty
      end
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

      it "does not relay a client reset during accept to rescuable_serve" do
        relayed = Queue.new
        accepts = 0
        allow(@server_transport).to receive(:accept).and_wrap_original do |accept|
          # The client went away before the server could accept it.
          raise Errno::ECONNRESET if (accepts += 1) == 1

          accept.call
        end
        caller_thread = in_server_threads do
          Thread.new do
            @pool_server.rescuable_serve
          rescue StandardError => e
            relayed << e
          end
        end
        port = Timeout.timeout(2) { @ready.pop }

        call_void_method(port)

        # A relayed error would end rescuable_serve in the caller's thread.
        expect(caller_thread.join(0.2)).to be_nil
        expect(relayed).to be_empty
        expect(@log.string).to be_empty
      end
    end
  end

  describe "when closing a client connection fails" do
    # Answers each 4-byte request with "pong" once the example releases it.
    class PongAfterReleaseProcessor
      def initialize(handled, release)
        @handled = handled
        @release = release
      end

      def process(iprot, oprot)
        iprot.trans.read_all(4)
        @handled << Thread.current
        @release.pop
        oprot.trans.write("pong")
        oprot.trans.flush
      end
    end

    class RecordingServerSocket < EphemeralServerSocket
      def initialize(ready, accepted)
        super(ready)
        @accepted = accepted
      end

      def accept
        super.tap { |client| @accepted << client.handle }
      end
    end

    before(:each) do
      @threads = ThreadGroup.new
      @ready = Queue.new
      @accepted = Queue.new
      @handled = Queue.new
      @release = Queue.new
      @errors = Queue.new
      @log = StringIO.new
      @processor = PongAfterReleaseProcessor.new(@handled, @release)
      @server_transport = RecordingServerSocket.new(@ready, @accepted)
      @transport_factory = Thrift::BufferedTransportFactory.new
    end

    after(:each) do
      until (threads = @threads.list).empty?
        threads.each(&:kill)
        threads.each(&:join)
      end
      @server_transport.close
    end

    def start_server(server)
      previous_group = Thread.current.group
      # Children inherit the group before their thread blocks can run.
      @threads.add(Thread.current)
      Thread.new do
        server.serve
      rescue StandardError => e
        @errors << e
      end
    ensure
      previous_group.add(Thread.current)
    end

    # The server's reply to the first client fails, so closing its buffered
    # transport cannot flush. Returns the threads that served each client.
    def reset_first_client_then_call_another(server)
      server_thread = start_server(server)
      port = Timeout.timeout(2) { @ready.pop }

      dropped = TCPSocket.new("127.0.0.1", port)
      dropped.write("ping")
      server_side = Timeout.timeout(2) { @accepted.pop }
      dropped_handler = Timeout.timeout(2) { @handled.pop }
      dropped.setsockopt(Socket::SOL_SOCKET, Socket::SO_LINGER, [1, 0].pack("ii"))
      dropped.close
      # Reply only once the server sees the reset.
      expect(IO.select([server_side], nil, nil, 2)).not_to be_nil
      @release << :reply

      second = TCPSocket.new("127.0.0.1", port)
      second.write("ping")
      @release << :reply
      expect(Timeout.timeout(2) { second.read(4) }).to eq("pong")
      [server_thread, dropped_handler, Timeout.timeout(2) { @handled.pop }]
    ensure
      dropped&.close
      second&.close
    end

    def expect_closed_stream_logged_at_debug
      expect(@log.string).to include(
        "DEBUG -- : Error while closing a client connection: #<Thrift::TransportException: closed stream>",
      )
    end

    it "keeps SimpleServer accepting clients" do
      server = Thrift::SimpleServer.new(@processor, @server_transport, @transport_factory, logger: Logger.new(@log))

      server_thread, = reset_first_client_then_call_another(server)

      expect(server_thread).to be_alive
      expect(@errors).to be_empty
      expect_closed_stream_logged_at_debug
    end

    it "ends the ThreadedServer connection thread without an exception" do
      server = Thrift::ThreadedServer.new(@processor, @server_transport, @transport_factory, logger: Logger.new(@log))

      server_thread, dropped_handler, = reset_first_client_then_call_another(server)

      expect(dropped_handler.join(2)).to equal(dropped_handler)
      expect(server_thread).to be_alive
      expect_closed_stream_logged_at_debug
    end

    it "keeps the ThreadPoolServer worker serving clients" do
      server = Thrift::ThreadPoolServer.new(@processor, @server_transport, @transport_factory, nil, 1, logger: Logger.new(@log))

      server_thread, dropped_handler, second_handler = reset_first_client_then_call_another(server)

      expect(second_handler).to equal(dropped_handler)
      expect(server_thread).to be_alive
      expect(@log.string).not_to include("Error in thread pool worker")
      expect_closed_stream_logged_at_debug
    end

    [Thrift::SimpleServer, Thrift::ThreadedServer, Thrift::ThreadPoolServer].each do |server_class|
      it "logs an unexpected #{server_class.name.split("::").last} close error at error level" do
        failing_close = Class.new(Thrift::BaseTransportFactory) do
          def get_transport(socket)
            Thrift::BufferedTransport.new(socket).tap do |transport|
              transport.define_singleton_method(:close) do
                super()
                raise "close bug"
              end
            end
          end
        end
        logged = Queue.new
        logger = Logger.new(@log)
        logger.formatter = proc do |severity, _, _, message|
          logged << "#{severity} #{message}"
          ""
        end
        start_server(server_class.new(@processor, @server_transport, failing_close.new, logger: logger))
        port = Timeout.timeout(2) { @ready.pop }

        client = TCPSocket.new("127.0.0.1", port)
        client.write("ping")
        @release << :reply
        expect(Timeout.timeout(2) { client.read(4) }).to eq("pong")
        client.close

        expect(Timeout.timeout(2) { logged.pop }).to eq(
          "ERROR Error while closing a client connection: #<RuntimeError: close bug>",
        )
      ensure
        client&.close
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

  describe "when accept fails" do
    # Ends a scripted run; none of the servers skips it.
    class AcceptScriptDone < StandardError; end

    # Plays back accept results: an exception class is raised, anything else
    # is returned. Once the script is used up, accept blocks. Once closed,
    # accept returns nil, as Thrift::ServerSocket#accept does.
    class ScriptedServerTransport
      def initialize(script)
        @script = script.dup
        @closed = false
      end

      def listen; end

      def close
        @closed = true
      end

      def closed?
        @closed
      end

      def accept
        return if @closed

        sleep if @script.empty?
        step = @script.shift
        raise step if step.is_a?(Class)

        step
      end
    end

    SERVER_KINDS = [:simple, :threaded, :thread_pool, :nonblocking].freeze

    before(:each) do
      @threads = ThreadGroup.new
      @log = StringIO.new
      @served = Queue.new
      @processor = double("processor")
      allow(@processor).to receive(:process) do |iprot, _oprot|
        @served << iprot.trans
        raise Thrift::TransportException
      end
    end

    after(:each) do
      until (threads = @threads.list).empty?
        threads.each(&:kill)
        threads.each(&:join)
      end
    end

    def build_server(kind, transport)
      logger = Logger.new(@log, level: Logger::WARN)
      case kind
      when :simple then Thrift::SimpleServer.new(@processor, transport, nil, nil, logger: logger)
      when :threaded then Thrift::ThreadedServer.new(@processor, transport, nil, nil, logger: logger)
      when :thread_pool then Thrift::ThreadPoolServer.new(@processor, transport, nil, nil, 1, logger: logger)
      when :nonblocking
        Thrift::NonblockingServer.new(@processor, transport, nil, nil, 1, logger).tap do |server|
          io_manager = double("IOManager", ensure_closed: nil, error: nil)
          allow(io_manager).to receive(:add_connection) { |socket| @served << socket }
          allow(server).to receive(:start_io_manager).and_return(io_manager)
          allow(server).to receive(:select).and_return([[transport], nil, nil])
        end
      end
    end

    # Serves the scripted accept results and returns the error that ended the
    # run (nil if it ended without one), the clients served, and the pauses.
    # Yields the transport at each pause.
    def run_script(kind, script, transport: ScriptedServerTransport.new(script))
      server = build_server(kind, transport)
      pauses = []
      allow(server).to receive(:sleep) do |seconds|
        pauses << seconds
        yield transport if block_given?
      end
      finished = Queue.new
      previous_group = Thread.current.group
      # Children inherit the group before their thread blocks can run.
      @threads.add(Thread.current)
      Thread.new do
        # Only rescuable_serve passes a pool worker's error to its caller.
        kind == :thread_pool ? server.rescuable_serve : server.serve
        finished << nil
      rescue StandardError => e
        finished << e
      end
      previous_group.add(Thread.current)
      error = Timeout.timeout(5) { finished.pop }
      clients = script.reject { |step| step.is_a?(Class) }
      served = Timeout.timeout(5) { Array.new(clients.size) { @served.pop } }
      [error, served, pauses]
    end

    def retry_log_lines
      @log.string.lines.grep(/Error while accepting a client connection, retrying/)
    end

    SERVER_KINDS.each do |kind|
      context "in a #{kind} server" do
        [Errno::ECONNRESET, Errno::EPIPE, Errno::ECONNABORTED, Errno::EPROTO].each do |error_class|
          it "skips #{error_class} at once and keeps accepting" do
            client = Thrift::MemoryBufferTransport.new
            error, served, pauses = run_script(kind, [error_class, client, AcceptScriptDone])

            expect(error).to be_a(AcceptScriptDone)
            expect(served).to eq([client])
            expect(pauses).to be_empty
            expect(@log.string).to be_empty
          end
        end

        [Errno::EMFILE, Errno::ENFILE, Errno::ENOBUFS, Errno::ENOMEM].each do |error_class|
          it "pauses after #{error_class}, logs it and keeps accepting" do
            client = Thrift::MemoryBufferTransport.new
            error, served, pauses = run_script(kind, [error_class, client, AcceptScriptDone])

            expect(error).to be_a(AcceptScriptDone)
            expect(served).to eq([client])
            expect(pauses).to eq([0.005])
            expect(retry_log_lines.size).to eq(1)
            expect(retry_log_lines.first).to include(error_class.name)
          end
        end

        it "doubles the pause up to its limit, logs once per run of failures and starts over after a client" do
          first = Thrift::MemoryBufferTransport.new
          second = Thrift::MemoryBufferTransport.new
          script = [*[Errno::EMFILE] * 10, first, Errno::ENFILE, Errno::EMFILE, second, AcceptScriptDone]
          error, served, pauses = run_script(kind, script)

          expect(error).to be_a(AcceptScriptDone)
          # ThreadedServer may run its handlers in either order.
          expect(served).to match_array([first, second])
          doubling = [0.005, 0.01, 0.02, 0.04, 0.08, 0.16, 0.32, 0.64, 1.28, 2.56].map { |pause| [pause, 1.0].min }
          expect(pauses).to eq([*doubling, 0.005, 0.01])
          expect(retry_log_lines.size).to eq(2)
        end

        it "stops on an accept error it does not name" do
          error, _served, pauses = run_script(kind, [Errno::EBADF])

          expect(error).to be_a(Errno::EBADF)
          expect(pauses).to be_empty
        end

        it "starts a new run of pauses after a connection that fails while it is accepted" do
          client = Thrift::MemoryBufferTransport.new
          script = [Errno::EMFILE, Errno::EMFILE, Errno::ECONNRESET, Errno::EMFILE, client, AcceptScriptDone]
          error, served, pauses = run_script(kind, script)

          expect(error).to be_a(AcceptScriptDone)
          expect(served).to eq([client])
          expect(pauses).to eq([0.005, 0.01, 0.005])
          expect(retry_log_lines.size).to eq(2)
        end
      end
    end

    it "ends SimpleServer and ThreadedServer when the transport is closed from another thread" do
      [:simple, :threaded].each do |kind|
        error, _served, _pauses = run_script(kind, [IOError])
        expect(error).to be_a(IOError)
      end
    end

    [:simple, :threaded, :thread_pool].each do |kind|
      it "ends a #{kind} server when the transport is closed during a pause" do
        error, _served, _pauses = run_script(kind, [Errno::EMFILE], &:close)

        expect(error).to be_a(IOError)
        expect(@processor).not_to have_received(:process)
      end
    end

    [Errno::ECONNRESET, Errno::EMFILE].each do |error_class|
      it "lets NonblockingServer see an IO manager failure after accept raises #{error_class}" do
        failure = RuntimeError.new("IO manager failed")
        server = build_server(:nonblocking, ScriptedServerTransport.new([error_class]))
        io_manager = double("IOManager", ensure_closed: nil)
        allow(io_manager).to receive(:error).and_return(nil, failure)
        allow(server).to receive(:start_io_manager).and_return(io_manager)
        allow(server).to receive(:sleep)

        expect { Timeout.timeout(2) { server.serve } }.to raise_error(failure)
      end
    end

    it "ends NonblockingServer without an error when the transport is closed" do
      error, _served, _pauses = run_script(:nonblocking, [IOError])
      expect(error).to be_nil
    end

    describe "when the process runs out of file descriptors" do
      # Runs in its own process so the descriptor limit stays out of rspec.
      SERVER_SCRIPT = <<~'RUBY'
        require "json"
        require "logger"
        require "socket"
        require "stringio"
        require "thrift"

        class EchoProcessor
          def process(iprot, oprot)
            oprot.trans.write(iprot.trans.read_all(4))
            oprot.trans.flush
          end
        end

        kind = ARGV[0]
        log = StringIO.new
        logger = Logger.new(log)
        transport = Thrift::ServerSocket.new("127.0.0.1", 0)
        server = case kind
        when "simple" then Thrift::SimpleServer.new(EchoProcessor.new, transport, nil, nil, logger: logger)
        when "threaded" then Thrift::ThreadedServer.new(EchoProcessor.new, transport, nil, nil, logger: logger)
        when "thread_pool" then Thrift::ThreadPoolServer.new(EchoProcessor.new, transport, nil, nil, 2, logger: logger)
        when "nonblocking"
          Thrift::NonblockingServer.new(EchoProcessor.new, transport, Thrift::FramedTransportFactory.new, nil, 1, logger)
        end
        request = kind == "nonblocking" ? [4].pack("N") + "ping" : "ping"
        call = lambda do |socket|
          socket.write(request)
          IO.select([socket], nil, nil, 5) ? socket.read(request.bytesize) : nil
        end

        Process.setrlimit(Process::RLIMIT_NOFILE, 64)
        server_thread = Thread.new { kind == "thread_pool" ? server.rescuable_serve : server.serve }
        server_thread.report_on_exception = false
        Thread.pass until transport.handle
        port = transport.handle.addr[1]
        TCPSocket.open("127.0.0.1", port) { |socket| call.(socket) }
        # Let the server close its end of that connection first.
        sleep 0.2

        # This client connects once no descriptor is left to accept it with.
        # Linux keeps it queued, but only the next client counts.
        waiting = Socket.new(:INET, :STREAM)
        # A collection could close a dropped socket and free a descriptor.
        GC.start
        GC.disable
        held = []
        begin
          loop { held << File.open(File::NULL) }
        rescue Errno::EMFILE
        end
        begin
          waiting.connect(Socket.sockaddr_in(port, "127.0.0.1"))
          waiting.write(request)
        rescue SystemCallError
        end
        sleep 0.5
        held.each(&:close)
        GC.enable
        begin
          IO.select([waiting], nil, nil, 2) && waiting.read(request.bytesize)
        rescue SystemCallError
        end
        waiting.close

        reply = begin
          TCPSocket.open("127.0.0.1", port) { |socket| call.(socket) }
        rescue SystemCallError => e
          e.inspect
        end
        error = begin
          server_thread.join(0.2)
          nil
        rescue Exception => e
          e.inspect
        end
        puts JSON.generate(
          reply: reply == request ? "echoed" : reply.inspect,
          serving: server_thread.alive?,
          error: error,
          retry_logs: log.string.scan("Errno::EMFILE").size,
        )
        $stdout.flush
        exit!(0)
      RUBY

      SERVER_KINDS.each do |kind|
        it "keeps a #{kind} server accepting once descriptors are free again" do
          # macOS drops the connection it cannot accept, and Ruby then waits
          # for another instead of raising EMFILE.
          skip "accept(2) reports EMFILE to Ruby only on Linux" unless RUBY_PLATFORM.include?("linux")

          lib = File.expand_path("../lib", __dir__)
          output = Timeout.timeout(30) do
            IO.popen([RbConfig.ruby, "-I", lib, "-e", SERVER_SCRIPT, kind.to_s], err: File::NULL, &:read)
          end
          result = JSON.parse(output.lines.last.to_s)

          expect(result).to include("reply" => "echoed", "serving" => true, "error" => nil)
          # One run of failures per accepting thread; the pool has two workers.
          expect(result["retry_logs"]).to be_between(1, kind == :thread_pool ? 2 : 1)
        end
      end
    end
  end
end
