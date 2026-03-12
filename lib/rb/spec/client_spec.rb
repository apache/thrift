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

describe 'Client' do
  class ClientSpec
    include Thrift::Client
  end

  class EmptyArgs
    def write(_prot)
    end
  end

  before(:each) do
    @prot = double("MockProtocol")
    @client = ClientSpec.new(@prot)
  end

  describe Thrift::Client do
    it "should re-use iprot for oprot if not otherwise specified" do
      expect(@client.instance_variable_get(:'@iprot')).to eql(@prot)
      expect(@client.instance_variable_get(:'@oprot')).to eql(@prot)
    end

    it "should send a test message" do
      expect(@prot).to receive(:write_message_begin).with('testMessage', Thrift::MessageTypes::CALL, 0)
      mock_args = double('#<TestMessage_args:mock>')
      expect(mock_args).to receive(:foo=).with('foo')
      expect(mock_args).to receive(:bar=).with(42)
      expect(mock_args).to receive(:write).with(@prot)
      expect(@prot).to receive(:write_message_end)
      expect(@prot).to receive(:trans) do
        double('trans').tap do |trans|
          expect(trans).to receive(:flush)
        end
      end
      klass = double("TestMessage_args", :new => mock_args)
      @client.send_message('testMessage', klass, :foo => 'foo', :bar => 42)
    end

    it "should increment the sequence id when sending messages" do
      expect(@prot).to receive(:write_message_begin).with('testMessage', Thrift::MessageTypes::CALL, 0).ordered
      expect(@prot).to receive(:write_message_begin).with('testMessage2', Thrift::MessageTypes::CALL, 1).ordered
      expect(@prot).to receive(:write_message_begin).with('testMessage3', Thrift::MessageTypes::CALL, 2).ordered
      allow(@prot).to receive(:write_message_end)
      allow(@prot).to receive(:trans).and_return(double("trans", :flush => nil))

      args_class = double("ArgsClass", :new => EmptyArgs.new)
      @client.send_message('testMessage', args_class)
      @client.send_message('testMessage2', args_class)
      @client.send_message('testMessage3', args_class)
    end

    it "should keep pending reply sequence ids in FIFO order" do
      expect(@prot).to receive(:write_message_begin).with('first', Thrift::MessageTypes::CALL, 0).ordered
      expect(@prot).to receive(:write_message_begin).with('second', Thrift::MessageTypes::CALL, 1).ordered
      allow(@prot).to receive(:write_message_end)
      allow(@prot).to receive(:trans).and_return(double("trans", :flush => nil))

      args_class = double("ArgsClass", :new => EmptyArgs.new)
      @client.send_message('first', args_class)
      @client.send_message('second', args_class)

      expect {
        @client.validate_message_begin('first', Thrift::MessageTypes::REPLY, 0, 'first')
      }.not_to raise_error
      expect {
        @client.validate_message_begin('second', Thrift::MessageTypes::REPLY, 1, 'second')
      }.not_to raise_error
    end

    it "should receive a test message" do
      expect(@prot).to receive(:read_message_begin).and_return [nil, Thrift::MessageTypes::CALL, 0]
      expect(@prot).to receive(:read_message_end)
      mock_klass = double("#<MockClass:mock>")
      expect(mock_klass).to receive(:read).with(@prot)
      @client.receive_message_begin()
      @client.receive_message(double("MockClass", :new => mock_klass))
    end

    it "should raise BAD_SEQUENCE_ID for mismatched replies" do
      @client.instance_variable_set(:@pending_seqids, [0])

      expect {
        @client.validate_message_begin('testMessage', Thrift::MessageTypes::REPLY, 1, 'testMessage')
      }.to raise_error(Thrift::ApplicationException) { |error|
        expect(error.type).to eq(Thrift::ApplicationException::BAD_SEQUENCE_ID)
      }
    end

    it "should raise WRONG_METHOD_NAME for unexpected replies" do
      @client.instance_variable_set(:@pending_seqids, [0])

      expect {
        @client.validate_message_begin('otherMessage', Thrift::MessageTypes::REPLY, 0, 'testMessage')
      }.to raise_error(Thrift::ApplicationException) { |error|
        expect(error.type).to eq(Thrift::ApplicationException::WRONG_METHOD_NAME)
      }
    end

    it "should raise INVALID_MESSAGE_TYPE for non-reply messages" do
      @client.instance_variable_set(:@pending_seqids, [0])

      expect {
        @client.validate_message_begin('testMessage', Thrift::MessageTypes::CALL, 0, 'testMessage')
      }.to raise_error(Thrift::ApplicationException) { |error|
        expect(error.type).to eq(Thrift::ApplicationException::INVALID_MESSAGE_TYPE)
      }
    end

    it "should raise received application exceptions" do
      expect(@prot).to receive(:read_message_end)
      server_exception = Thrift::ApplicationException.new(Thrift::ApplicationException::UNKNOWN, "boom")
      expect(server_exception).to receive(:read).with(@prot)
      expect(Thrift::ApplicationException).to receive(:new).and_return(server_exception)
      @client.instance_variable_set(:@pending_seqids, [0])

      expect {
        @client.validate_message_begin('testMessage', Thrift::MessageTypes::EXCEPTION, 0, 'testMessage')
      }.to raise_error(Thrift::ApplicationException, "boom")
      expect(@client.instance_variable_get(:@pending_seqids)).to be_empty
    end

    it "should roll sequence ids across the signed int32 boundary" do
      expect(@prot).to receive(:write_message_begin).with('testMessage', Thrift::MessageTypes::CALL, Thrift::Client::MAX_SEQUENCE_ID).ordered
      expect(@prot).to receive(:write_message_begin).with('testMessage2', Thrift::MessageTypes::CALL, Thrift::Client::MIN_SEQUENCE_ID).ordered
      allow(@prot).to receive(:write_message_end)
      allow(@prot).to receive(:trans).and_return(double("trans", :flush => nil))

      @client.instance_variable_set(:@seqid, Thrift::Client::MAX_SEQUENCE_ID)
      args_class = double("ArgsClass", :new => EmptyArgs.new)
      @client.send_message('testMessage', args_class)
      @client.send_message('testMessage2', args_class)
    end

    it "should close the transport if an error occurs while sending a message" do
      allow(@prot).to receive(:write_message_begin)
      expect(@prot).not_to receive(:write_message_end)
      mock_args = double("#<TestMessage_args:mock>")
      expect(mock_args).to receive(:write).with(@prot).and_raise(StandardError)
      trans = double("MockTransport")
      allow(@prot).to receive(:trans).and_return(trans)
      expect(trans).to receive(:close)
      klass = double("TestMessage_args", :new => mock_args)
      expect { @client.send_message("testMessage", klass) }.to raise_error(StandardError)
    end
  end
end
