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

describe "Processor" do
  class ProcessorSpec
    include Thrift::Processor

    attr_reader :processed

    def process_work(seqid, iprot, _oprot)
      iprot.skip(Thrift::Types::STRUCT)
      iprot.read_message_end
      @processed = seqid
    end
  end

  describe Thrift::Processor do
    before(:each) do
      @processor = ProcessorSpec.new(double("MockHandler"))
      @prot = double("MockProtocol")
    end

    def mock_trans(obj)
      expect(obj).to receive(:trans).ordered do
        double("trans").tap do |trans|
          expect(trans).to receive(:flush).ordered
        end
      end
    end

    def input_protocol(name, type, seqid, args = nil)
      transport = Thrift::MemoryBufferTransport.new
      protocol = Thrift::BinaryProtocol.new(transport)
      protocol.write_message_begin(name, type, seqid)
      if args
        args.write(protocol)
      else
        protocol.write_struct_begin("args")
        protocol.write_field_stop
        protocol.write_struct_end
      end
      protocol.write_message_end
      Thrift::BinaryProtocol.new(transport)
    end

    def output_protocol
      transport = Thrift::MemoryBufferTransport.new
      [transport, Thrift::BinaryProtocol.new(transport)]
    end

    it "should call process_<message> when it receives that message" do
      expect(@prot).to receive(:read_message_begin).ordered.and_return ["testMessage", Thrift::MessageTypes::CALL, 17]
      expect(@processor).to receive(:process_testMessage).with(17, @prot, @prot).ordered
      expect(@processor.process(@prot, @prot)).to eq(true)
    end

    [Thrift::MessageTypes::REPLY, Thrift::MessageTypes::EXCEPTION].each do |message_type|
      it "rejects message type #{message_type} before dispatching" do
        input = input_protocol("work", message_type, 11)
        output_transport, output = output_protocol

        expect(@processor.process(input, output)).to be false
        expect(@processor.processed).to be_nil

        response = Thrift::BinaryProtocol.new(output_transport)
        name, type, seqid = response.read_message_begin
        exception = Thrift::ApplicationException.new
        exception.read(response)
        response.read_message_end

        expect(name).to eq("work")
        expect(type).to eq(Thrift::MessageTypes::EXCEPTION)
        expect(seqid).to eq(11)
        expect(exception.type).to eq(Thrift::ApplicationException::INVALID_MESSAGE_TYPE)
        expect(exception.message).to eq("Invalid message type #{message_type} for function work")
      end
    end

    [Thrift::MessageTypes::CALL, Thrift::MessageTypes::ONEWAY].each do |message_type|
      it "dispatches valid message type #{message_type}" do
        input = input_protocol("work", message_type, 12)
        output_transport, output = output_protocol

        expect(@processor.process(input, output)).to be true
        expect(@processor.processed).to eq(12)
        expect(output_transport.available).to eq(0)
      end
    end

    it "keeps generated oneway behavior when its envelope is CALL" do
      handler = double("Handler")
      expect(handler).to receive(:unblock).with(9)
      processor = SpecNamespace::NonblockingService::Processor.new(handler)
      args = SpecNamespace::NonblockingService::Unblock_args.new(n: 9)
      input = input_protocol("unblock", Thrift::MessageTypes::CALL, 13, args)
      output_transport, output = output_protocol

      expect(processor.process(input, output)).to be true
      expect(output_transport.available).to eq(0)
    end

    it "keeps generated reply behavior when a normal method envelope is ONEWAY" do
      handler = double("Handler")
      expect(handler).to receive(:sleep).with(3.0)
      processor = SpecNamespace::NonblockingService::Processor.new(handler)
      args = SpecNamespace::NonblockingService::Sleep_args.new(seconds: 3.0)
      input = input_protocol("sleep", Thrift::MessageTypes::ONEWAY, 14, args)
      output_transport, output = output_protocol

      expect(processor.process(input, output)).to be true

      response = Thrift::BinaryProtocol.new(output_transport)
      expect(response.read_message_begin).to eq(["sleep", Thrift::MessageTypes::REPLY, 14])
      response.skip(Thrift::Types::STRUCT)
      response.read_message_end
    end

    it "should raise an ApplicationException when the received message cannot be processed" do
      expect(@prot).to receive(:read_message_begin).ordered.and_return ["testMessage", Thrift::MessageTypes::CALL, 4]
      expect(@prot).to receive(:skip).with(Thrift::Types::STRUCT).ordered
      expect(@prot).to receive(:read_message_end).ordered
      expect(@prot).to receive(:write_message_begin).with("testMessage", Thrift::MessageTypes::EXCEPTION, 4).ordered
      e = double(Thrift::ApplicationException)
      expect(e).to receive(:write).with(@prot).ordered
      expect(Thrift::ApplicationException).to receive(:new).with(Thrift::ApplicationException::UNKNOWN_METHOD, "Unknown function testMessage").and_return(e)
      expect(@prot).to receive(:write_message_end).ordered
      mock_trans(@prot)
      @processor.process(@prot, @prot)
    end

    it "should pass args off to the args class" do
      args_class = double("MockArgsClass")
      args = double("#<MockArgsClass:mock>").tap do |args|
        expect(args).to receive(:read).with(@prot).ordered
      end
      expect(args_class).to receive(:new).and_return args
      expect(@prot).to receive(:read_message_end).ordered
      expect(@processor.read_args(@prot, args_class)).to eql(args)
    end

    it "classifies malformed JSON binary arguments as invalid protocol data" do
      input = Thrift::JsonProtocol.new(
        Thrift::MemoryBufferTransport.new('{"1":{"str":"%"}}'),
      )

      expect { @processor.read_args(input, SpecNamespace::Foo2) }.to raise_error(Thrift::ProtocolException) do |error|
        expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
      end
    end

    it "should write out a reply when asked" do
      expect(@prot).to receive(:write_message_begin).with("testMessage", Thrift::MessageTypes::REPLY, 23).ordered
      result = double("MockResult")
      expect(result).to receive(:write).with(@prot).ordered
      expect(@prot).to receive(:write_message_end).ordered
      mock_trans(@prot)
      @processor.write_result(result, @prot, "testMessage", 23)
    end
  end
end
