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
require 'openssl'
require 'timeout'

describe 'Server' do
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
      expect { @server.serve() }.to raise_error(NotImplementedError)
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
        super('127.0.0.1', 0)
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

    {
      Thrift::CompactProtocolFactory.new => proc do
        trans = Thrift::MemoryBufferTransport.new
        prot = Thrift::CompactProtocol.new(trans)
        prot.write_message_begin('unknown', Thrift::MessageTypes::CALL, 1)
        trans.write([0x1e, 0].pack('C*'))
        trans.read(trans.available)
      end,
      Thrift::JsonProtocolFactory.new => proc { '[1,"unknown",1,1,{"1":{"wat":0}}]' }
    }.each do |protocol_factory, malformed_request|
      it "closes a malformed #{protocol_factory} connection and continues accepting clients" do
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
        malformed_client = TCPSocket.new('127.0.0.1', port)
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

        valid_transport = Thrift::Socket.new('127.0.0.1', port)
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
  end

  describe Thrift::ThreadedServer do
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
        exception_q.push(StandardError.new('ERROR'))
      end
      expect { @server.rescuable_serve }.to(raise_error('ERROR'))
      sleep(0.15)
    end

    it "should avoid running the server twice when retrying rescuable_serve" do
      exception_q = @server.instance_variable_get(:@exception_q)
      expect_any_instance_of(described_class).to receive(:serve) do
        exception_q.push(StandardError.new('ERROR1'))
        exception_q.push(StandardError.new('ERROR2'))
      end
      expect { @server.rescuable_serve }.to(raise_error('ERROR1'))
      expect { @server.rescuable_serve }.to(raise_error('ERROR2'))
    end

    it "should serve using a thread pool" do
      thread_q = double("SizedQueue")
      exception_q = double("Queue")
      @server.instance_variable_set(:@thread_q, thread_q)
      @server.instance_variable_set(:@exception_q, exception_q)
      expect(@server_trans).to receive(:listen).ordered
      expect(thread_q).to receive(:push).with(:token)
      expect(thread_q).to receive(:pop)
      expect(Thread).to receive(:new).and_yield
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
      expect(exception_q).to receive(:push).with(error).and_throw(:stop)
      expect(@server_trans).to receive(:close)
      expect { @server.serve }.to(throw_symbol(:stop))
    end

    it "should not enqueue TLS accept errors" do
      exception_q = @server.instance_variable_get(:@exception_q)
      expect(@server_trans).to receive(:listen).ordered
      expect(@server_trans).to receive(:accept).ordered.and_raise(OpenSSL::SSL::SSLError)
      expect(@server_trans).to receive(:accept).ordered.and_return(@client)
      expect(@trans).to receive(:get_transport).once.with(@client).and_return(@trans)
      expect(@prot).to receive(:get_protocol).once.with(@trans).and_return(@prot)
      allow(Thread).to receive(:new).and_yield
      expect(@processor).to receive(:process).once.with(@prot, @prot) { throw :stop }
      expect(@trans).to receive(:close).once
      expect(@server_trans).to receive(:close)

      catch(:stop) { @server.serve }

      expect(exception_q).to be_empty
    end
  end
end
