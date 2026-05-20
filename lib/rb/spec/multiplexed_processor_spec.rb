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

describe Thrift::MultiplexedProcessor do
  before(:each) do
    @processor = Thrift::MultiplexedProcessor.new
    @iprot = double('MockInputProtocol')
    @oprot = double('MockOutputProtocol')
  end

  it 'dispatches multiplexed calls to the registered service processor' do
    actual_processor = double('ActualProcessor')
    @processor.register_processor('ThriftTest', actual_processor)

    expect(@iprot).to receive(:read_message_begin).and_return(['ThriftTest:testVoid', Thrift::MessageTypes::CALL, 1])
    expect(actual_processor).to receive(:process) do |stored_protocol, oprot|
      expect(stored_protocol.read_message_begin).to eq(['testVoid', Thrift::MessageTypes::CALL, 1])
      expect(oprot).to eq(@oprot)
      true
    end

    expect(@processor.process(@iprot, @oprot)).to eq(true)
  end

  it 'dispatches non-multiplexed calls to the default processor' do
    default_processor = double('DefaultProcessor')
    @processor.register_default(default_processor)

    expect(@iprot).to receive(:read_message_begin).and_return(['testVoid', Thrift::MessageTypes::CALL, 2])
    expect(default_processor).to receive(:process) do |stored_protocol, oprot|
      expect(stored_protocol.read_message_begin).to eq(['testVoid', Thrift::MessageTypes::CALL, 2])
      expect(oprot).to eq(@oprot)
      true
    end

    expect(@processor.process(@iprot, @oprot)).to eq(true)
  end

  it 'raises for non-multiplexed calls when no default processor is registered' do
    expect(@iprot).to receive(:read_message_begin).and_return(['testVoid', Thrift::MessageTypes::CALL, 3])

    expect { @processor.process(@iprot, @oprot) }.to raise_error(
      Thrift::Exception,
      'Service name not found in message name: testVoid. Did you forget to use a Thrift::Protocol::MultiplexedProtocol in your client?'
    )
  end

  it 'raises for unknown multiplexed service names' do
    expect(@iprot).to receive(:read_message_begin).and_return(['Missing:testVoid', Thrift::MessageTypes::CALL, 4])

    expect { @processor.process(@iprot, @oprot) }.to raise_error(
      Thrift::Exception,
      'Service name not found: Missing. Did you forget to call Thrift::MultiplexedProcessor#register_processor?'
    )
  end
end
