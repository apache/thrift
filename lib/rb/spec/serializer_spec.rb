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

module SerializerFailureFixtures
  class Value
    def initialize(stage, error)
      @stage = stage
      @error = error
    end

    def write(protocol)
      case @stage
      when :message
        protocol.write_message_begin("broken", Thrift::MessageTypes::CALL, 1)
      when :struct
        protocol.write_struct_begin("Broken")
      when :field
        protocol.write_struct_begin("Broken")
        protocol.write_field_begin("value", Thrift::Types::STRING, 1)
      when :list
        protocol.write_struct_begin("Broken")
        protocol.write_field_begin("value", Thrift::Types::LIST, 1)
        protocol.write_list_begin(Thrift::Types::STRING, 1)
      when :set
        protocol.write_struct_begin("Broken")
        protocol.write_field_begin("value", Thrift::Types::SET, 1)
        protocol.write_set_begin(Thrift::Types::STRING, 1)
      when :nested_map
        protocol.write_struct_begin("Broken")
        protocol.write_field_begin("value", Thrift::Types::MAP, 1)
        protocol.write_map_begin(Thrift::Types::STRING, Thrift::Types::LIST, 1)
        protocol.write_string("key")
        protocol.write_list_begin(Thrift::Types::STRING, 1)
      end
      raise @error
    end
  end
end

describe 'Serializer' do
  describe Thrift::Serializer do
    it "should serialize structs to binary by default" do
      serializer = Thrift::Serializer.new(Thrift::BinaryProtocolAcceleratedFactory.new)
      data = serializer.serialize(SpecNamespace::Hello.new(:greeting => "'Ello guv'nor!"))
      expect(data).to eq("\x0B\x00\x01\x00\x00\x00\x0E'Ello guv'nor!\x00")
    end

    it "should serialize structs to the given protocol" do
      protocol = Thrift::BaseProtocol.new(double("transport"))
      expect(protocol).to receive(:write_struct_begin).with("SpecNamespace::Hello")
      expect(protocol).to receive(:write_field_begin).with("greeting", Thrift::Types::STRING, 1)
      expect(protocol).to receive(:write_string).with("Good day")
      expect(protocol).to receive(:write_field_end)
      expect(protocol).to receive(:write_field_stop)
      expect(protocol).to receive(:write_struct_end)
      protocol_factory = double("ProtocolFactory")
      allow(protocol_factory).to receive(:get_protocol).and_return(protocol)
      serializer = Thrift::Serializer.new(protocol_factory)
      serializer.serialize(SpecNamespace::Hello.new(:greeting => "Good day"))
    end

    [:message, :struct, :field, :list, :set, :nested_map].each do |stage|
      it "isolates JSON protocol state after a #{stage} write failure" do
        serializer = Thrift::Serializer.new(Thrift::JsonProtocolFactory.new)
        value = SpecNamespace::Hello.new(:greeting => "Good day")
        expected = Thrift::Serializer.new(Thrift::JsonProtocolFactory.new).serialize(value)
        error = RuntimeError.new("failed at #{stage}")

        expect {
          serializer.serialize(SerializerFailureFixtures::Value.new(stage, error))
        }.to raise_error { |raised| expect(raised).to equal(error) }

        2.times do
          data = serializer.serialize(value)
          expect(data).to eq(expected)
          expect(
            Thrift::Deserializer.new(Thrift::JsonProtocolFactory.new).deserialize(
              SpecNamespace::Hello.new,
              data
            )
          ).to eq(value)
        end
      end
    end

    it "recovers after repeated JSON serialization failures" do
      serializer = Thrift::Serializer.new(Thrift::JsonProtocolFactory.new)
      value = SpecNamespace::Hello.new(:greeting => "recovered")

      [:field, :nested_map].each do |stage|
        error = RuntimeError.new("failed at #{stage}")
        expect {
          serializer.serialize(SerializerFailureFixtures::Value.new(stage, error))
        }.to raise_error { |raised| expect(raised).to equal(error) }
      end

      expect(serializer.serialize(value)).to eq(
        Thrift::Serializer.new(Thrift::JsonProtocolFactory.new).serialize(value)
      )
    end
  end

  describe Thrift::Deserializer do
    it "should deserialize structs from binary by default" do
      deserializer = Thrift::Deserializer.new
      data = "\x0B\x00\x01\x00\x00\x00\x0E'Ello guv'nor!\x00"
      expect(deserializer.deserialize(SpecNamespace::Hello.new, data)).to eq(SpecNamespace::Hello.new(:greeting => "'Ello guv'nor!"))
    end

    it "should deserialize structs from the given protocol" do
      protocol = Thrift::BaseProtocol.new(double("transport"))
      expect(protocol).to receive(:read_struct_begin).and_return("SpecNamespace::Hello")
      expect(protocol).to receive(:read_field_begin).and_return(["greeting", Thrift::Types::STRING, 1],
                                                            [nil, Thrift::Types::STOP, 0])
      expect(protocol).to receive(:read_string).and_return("Good day")
      expect(protocol).to receive(:read_field_end)
      expect(protocol).to receive(:read_struct_end)
      protocol_factory = double("ProtocolFactory")
      allow(protocol_factory).to receive(:get_protocol).and_return(protocol)
      deserializer = Thrift::Deserializer.new(protocol_factory)
      expect(deserializer.deserialize(SpecNamespace::Hello.new, "")).to eq(SpecNamespace::Hello.new(:greeting => "Good day"))
    end
  end
end
