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

require File.dirname(__FILE__) + '/spec_helper'

class ThriftServerSpec < Spec::ExampleGroup
  include Thrift

  describe BaseServer do
    it "should default to BaseTransportFactory and BinaryProtocolFactory when not specified" do
      server = BaseServer.new(mock("Processor"), mock("BaseServerTransport"))
      server.instance_variable_get(:'@transport_factory').should be_an_instance_of(BaseTransportFactory)
      server.instance_variable_get(:'@protocol_factory').should be_an_instance_of(BinaryProtocolFactory)
    end

    # serve is a noop, so can't test that
  end

  shared_examples_for "servers" do
    before(:each) do
      @processor = mock("Processor")
      @serverTrans = mock("ServerTransport")
      @trans = mock("BaseTransport")
      @prot = mock("BaseProtocol")
      @client = mock("Client")
      @server = server_type.new(@processor, @serverTrans, @trans, @prot)
    end
  end

  describe SimpleServer do
    it_should_behave_like "servers"

    def server_type
      SimpleServer
    end

    it "should serve in the main thread" do
      @serverTrans.should_receive(:listen).ordered
      @serverTrans.should_receive(:accept).exactly(3).times.and_return(@client)
      @trans.should_receive(:get_transport).exactly(3).times.with(@client).and_return(@trans)
      @prot.should_receive(:get_protocol).exactly(3).times.with(@trans).and_return(@prot)
      x = 0
      @processor.should_receive(:process).exactly(3).times.with(@prot, @prot).and_return do
        case (x += 1)
        when 1 then raise Thrift::TransportException
        when 2 then raise Thrift::ProtocolException
        when 3 then throw :stop
        end
      end
      @trans.should_receive(:close).exactly(3).times
      @serverTrans.should_receive(:close).ordered
      lambda { @server.serve }.should throw_symbol(:stop)
    end
  end

  describe ThreadedServer do
    it_should_behave_like "servers"

    def server_type
      ThreadedServer
    end

    it "should serve using threads" do
      @serverTrans.should_receive(:listen).ordered
      @serverTrans.should_receive(:accept).exactly(3).times.and_return(@client)
      @trans.should_receive(:get_transport).exactly(3).times.with(@client).and_return(@trans)
      @prot.should_receive(:get_protocol).exactly(3).times.with(@trans).and_return(@prot)
      Thread.should_receive(:new).with(@prot, @trans).exactly(3).times.and_yield(@prot, @trans)
      x = 0
      @processor.should_receive(:process).exactly(3).times.with(@prot, @prot).and_return do
        case (x += 1)
        when 1 then raise Thrift::TransportException
        when 2 then raise Thrift::ProtocolException
        when 3 then throw :stop
        end
      end
      @trans.should_receive(:close).exactly(3).times
      @serverTrans.should_receive(:close).ordered
      lambda { @server.serve }.should throw_symbol(:stop)
    end
  end

  describe ThreadPoolServer do
    it_should_behave_like "servers"

    def server_type
      # put this stuff here so it runs before the server is created
      @threadQ = mock("SizedQueue")
      SizedQueue.should_receive(:new).with(20).and_return(@threadQ)
      @excQ = mock("Queue")
      Queue.should_receive(:new).and_return(@excQ)
      ThreadPoolServer
    end

    it "should set up the queues" do
      @server.instance_variable_get(:'@thread_q').should be(@threadQ)
      @server.instance_variable_get(:'@exception_q').should be(@excQ)
    end

    it "should serve inside a thread" do
      Thread.should_receive(:new).and_return do |block|
        @server.should_receive(:serve)
        block.call
        @server.rspec_verify
      end
      @excQ.should_receive(:pop).and_throw(:popped)
      lambda { @server.rescuable_serve }.should throw_symbol(:popped)
    end

    it "should avoid running the server twice when retrying rescuable_serve" do
      Thread.should_receive(:new).and_return do |block|
        @server.should_receive(:serve)
        block.call
        @server.rspec_verify
      end
      @excQ.should_receive(:pop).twice.and_throw(:popped)
      lambda { @server.rescuable_serve }.should throw_symbol(:popped)
      lambda { @server.rescuable_serve }.should throw_symbol(:popped)
    end

    it "should serve using a thread pool" do
      @serverTrans.should_receive(:listen).ordered
      @threadQ.should_receive(:push).with(:token)
      @threadQ.should_receive(:pop)
      Thread.should_receive(:new).and_yield
      @serverTrans.should_receive(:accept).exactly(3).times.and_return(@client)
      @trans.should_receive(:get_transport).exactly(3).times.and_return(@trans)
      @prot.should_receive(:get_protocol).exactly(3).times.and_return(@prot)
      x = 0
      error = RuntimeError.new("Stopped")
      @processor.should_receive(:process).exactly(3).times.with(@prot, @prot).and_return do
        case (x += 1)
        when 1 then raise Thrift::TransportException
        when 2 then raise Thrift::ProtocolException
        when 3 then raise error
        end
      end
      @trans.should_receive(:close).exactly(3).times
      @excQ.should_receive(:push).with(error).and_throw(:stop)
      @serverTrans.should_receive(:close)
      lambda { @server.serve }.should throw_symbol(:stop)
    end
  end
end
