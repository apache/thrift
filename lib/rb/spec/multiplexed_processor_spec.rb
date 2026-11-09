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
require "thrift/server/rack_application"

describe Thrift::MultiplexedProcessor do
  before(:each) do
    @processor = Thrift::MultiplexedProcessor.new
    @iprot = double("MockInputProtocol")
    @oprot = double("MockOutputProtocol")
  end

  it "dispatches multiplexed calls to the registered service processor" do
    actual_processor = double("ActualProcessor")
    @processor.register_processor("ThriftTest", actual_processor)

    expect(@iprot).to receive(:read_message_begin).and_return(["ThriftTest:testVoid", Thrift::MessageTypes::CALL, 1])
    expect(actual_processor).to receive(:process) do |stored_protocol, oprot|
      expect(stored_protocol.read_message_begin).to eq(["testVoid", Thrift::MessageTypes::CALL, 1])
      expect(oprot).to eq(@oprot)
      true
    end

    expect(@processor.process(@iprot, @oprot)).to eq(true)
  end

  it "dispatches non-multiplexed calls to the default processor" do
    default_processor = double("DefaultProcessor")
    @processor.register_default(default_processor)

    expect(@iprot).to receive(:read_message_begin).and_return(["testVoid", Thrift::MessageTypes::CALL, 2])
    expect(default_processor).to receive(:process) do |stored_protocol, oprot|
      expect(stored_protocol.read_message_begin).to eq(["testVoid", Thrift::MessageTypes::CALL, 2])
      expect(oprot).to eq(@oprot)
      true
    end

    expect(@processor.process(@iprot, @oprot)).to eq(true)
  end

  ["Service:", ""].each do |prefix|
    context "Rack error handling with #{prefix.empty? ? "default" : "named"} routing" do
      let(:handler) { double("Handler") }

      before do
        actual_processor = SpecNamespace::NonblockingService::Processor.new(handler)
        @processor.register_processor("Service", actual_processor)
        @processor.register_default(actual_processor)
      end

      [Thrift::MessageTypes::CALL, Thrift::MessageTypes::ONEWAY].each do |type|
        it "reports the original handler failure and respects envelope #{type}" do
          error = RuntimeError.new("handler failed")
          expect(handler).to receive(:shutdown).and_raise(error)
          transport = Thrift::MemoryBufferTransport.new
          protocol = Thrift::BinaryProtocol.new(transport)
          protocol.write_message_begin("#{prefix}shutdown", type, 19)
          SpecNamespace::NonblockingService::Shutdown_args.new.write(protocol)
          protocol.write_message_end
          env = Rack::MockRequest.env_for("/", method: "POST", "CONTENT_TYPE" => "application/x-thrift", input: transport.read(transport.available))
          response = Rack::MockResponse.new(*Thrift::RackApplication.new(@processor).call(env))

          expect(response.status).to eq 200
          expect(env["rack.exception"]).to equal error
          if type == Thrift::MessageTypes::ONEWAY
            expect(response.body).to be_empty
          else
            protocol = Thrift::BinaryProtocol.new(Thrift::MemoryBufferTransport.new(response.body))
            expect(protocol.read_message_begin).to eq ["shutdown", Thrift::MessageTypes::EXCEPTION, 19]
            exception = Thrift::ApplicationException.new
            exception.read(protocol)
            expect(exception.type).to eq Thrift::ApplicationException::INTERNAL_ERROR
          end
        end
      end

      [Thrift::BinaryProtocolFactory, Thrift::HeaderProtocolFactory].each do |factory_class|
        [Thrift::MessageTypes::CALL, Thrift::MessageTypes::ONEWAY].each do |type|
          it "returns an unreported 400 for truncated #{factory_class} arguments with envelope #{type}" do
            transport = Thrift::MemoryBufferTransport.new
            Thrift::BinaryProtocol.new(transport).write_message_begin("#{prefix}shutdown", type, 19)
            expect(handler).not_to receive(:shutdown)
            env = Rack::MockRequest.env_for("/", method: "POST", "CONTENT_TYPE" => "application/x-thrift", input: transport.read(transport.available))
            response = Rack::MockResponse.new(*Thrift::RackApplication.new(@processor, factory_class.new).call(env))

            expect(response.status).to eq 400
            expect(response.headers["Content-Type"]).to eq "application/x-thrift"
            expect(response.body).to be_empty
            expect(env["rack.exception"]).to be_nil
          end
        end
      end
    end
  end

  it "returns an unreported 400 for a malformed Header frame before routing" do
    env = Rack::MockRequest.env_for("/", method: "POST", "CONTENT_TYPE" => "application/x-thrift", input: [8, 0x12345678, 0].pack("N*"))
    response = Rack::MockResponse.new(*Thrift::RackApplication.new(@processor, Thrift::HeaderProtocolFactory.new).call(env))

    expect(response.status).to eq 400
    expect(response.headers["Content-Type"]).to eq "application/x-thrift"
    expect(response.body).to be_empty
    expect(env["rack.exception"]).to be_nil
  end

  it "raises for non-multiplexed calls when no default processor is registered" do
    expect(@iprot).to receive(:read_message_begin).and_return(["testVoid", Thrift::MessageTypes::CALL, 3])

    expect { @processor.process(@iprot, @oprot) }.to raise_error(
      Thrift::Exception,
      "Service name not found in message name: testVoid. Did you forget to use a Thrift::Protocol::MultiplexedProtocol in your client?",
    )
  end

  it "raises for unknown multiplexed service names" do
    expect(@iprot).to receive(:read_message_begin).and_return(["Missing:testVoid", Thrift::MessageTypes::CALL, 4])

    expect { @processor.process(@iprot, @oprot) }.to raise_error(
      Thrift::Exception,
      "Service name not found: Missing. Did you forget to call Thrift::MultiplexedProcessor#register_processor?",
    )
  end

  it "dispatches a service name that is not ASCII to its registered processor" do
    actual_processor = double("ActualProcessor")
    @processor.register_processor("Sérvice", actual_processor)

    expect(@iprot).to receive(:read_message_begin).and_return(["Sérvice:tést", Thrift::MessageTypes::CALL, 5])
    expect(actual_processor).to receive(:process) do |stored_protocol, _oprot|
      expect(stored_protocol.read_message_begin).to eq(["tést", Thrift::MessageTypes::CALL, 5])
      true
    end

    expect(@processor.process(@iprot, @oprot)).to eq(true)
  end

  context "with a name that is not valid UTF-8" do
    def process_call(name, seqid)
      input_transport = Thrift::MemoryBufferTransport.new
      request = Thrift::BinaryProtocol.new(input_transport)
      request.write_message_begin(name.b.force_encoding(Encoding::UTF_8), Thrift::MessageTypes::CALL, seqid)
      request.write_struct_begin("args")
      request.write_field_stop
      request.write_struct_end
      request.write_message_end
      output_transport = Thrift::MemoryBufferTransport.new

      result = @processor.process(Thrift::BinaryProtocol.new(input_transport), Thrift::BinaryProtocol.new(output_transport))

      response = Thrift::BinaryProtocol.new(output_transport)
      reply_name, type, reply_seqid = response.read_message_begin
      exception = Thrift::ApplicationException.new
      exception.read(response)
      response.read_message_end
      [result, reply_name.b, type, reply_seqid, exception.type]
    end

    it "answers an invalid method name with UNKNOWN_METHOD from the service processor" do
      @processor.register_processor("Srv", Thrift::Test::Srv::Processor.new(double("Handler")))

      expect(process_call("Srv:void\xFFMethod", 6)).to eq(
        [false, "void\xFFMethod".b, Thrift::MessageTypes::EXCEPTION, 6, Thrift::ApplicationException::UNKNOWN_METHOD],
      )
    end

    it "answers an invalid non-multiplexed name with UNKNOWN_METHOD from the default processor" do
      @processor.register_default(Thrift::Test::Srv::Processor.new(double("Handler")))

      expect(process_call("void\xFFMethod", 7)).to eq(
        [false, "void\xFFMethod".b, Thrift::MessageTypes::EXCEPTION, 7, Thrift::ApplicationException::UNKNOWN_METHOD],
      )
    end

    it "raises for an invalid service name as for any unknown service" do
      name = "\xFFMissing:testVoid".b.force_encoding(Encoding::UTF_8)
      expect(@iprot).to receive(:read_message_begin).and_return([name, Thrift::MessageTypes::CALL, 8])

      expect { @processor.process(@iprot, @oprot) }.to raise_error(Thrift::Exception) do |error|
        expect(error.message.b).to eq(
          "Service name not found: \xFFMissing. Did you forget to call Thrift::MultiplexedProcessor#register_processor?".b,
        )
      end
    end
  end
end
