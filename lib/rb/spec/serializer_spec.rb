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

module DeserializerResetFixtures
  class ResetFieldsStruct
    include Thrift::Struct, Thrift::Struct_Union

    FIELDS = {
      1 => {type: Thrift::Types::STRING, name: "reset_fields", optional: true}
    }.freeze

    def struct_fields
      FIELDS
    end

    def validate
    end

    Thrift::Struct.generate_accessors(self)
  end

  class RequiredStruct
    include Thrift::Struct, Thrift::Struct_Union

    FIELDS = {
      1 => {type: Thrift::Types::STRING, name: "required_value"}
    }.freeze

    def struct_fields
      FIELDS
    end

    def validate
      raise Thrift::ProtocolException.new(Thrift::ProtocolException::INVALID_DATA, "Required field required_value is unset!") unless @required_value
    end

    Thrift::Struct.generate_accessors(self)
  end

  class ResetFieldsUnion < Thrift::Union
    include Thrift::Struct_Union

    FIELDS = {
      1 => {type: Thrift::Types::STRING, name: "reset_fields", optional: true}
    }.freeze

    def struct_fields
      FIELDS
    end

    def validate
      raise Thrift::ProtocolException.new(Thrift::ProtocolException::INVALID_DATA, "Union fields are not set.") if get_set_field.nil? || get_value.nil?
    end

    Thrift::Union.generate_accessors(self)
  end
end

module SerializerFixtures
  class HeaderMessage
    def initialize(value, sequence_id)
      @value = value
      @sequence_id = sequence_id
    end

    def write(protocol)
      protocol.set_header("request-id", "abc")
      protocol.add_transform(Thrift::HeaderTransformID::ZLIB)
      protocol.write_message_begin("serialize", Thrift::MessageTypes::CALL, @sequence_id)
      @value.write(protocol)
      protocol.write_message_end
    end
  end

  class FramedBinaryProtocolFactory
    def get_protocol(transport)
      Thrift::BinaryProtocol.new(Thrift::FramedTransport.new(transport))
    end
  end
end

describe "Serializer" do
  describe Thrift::Serializer do
    it "should serialize structs to binary by default" do
      serializer = Thrift::Serializer.new(Thrift::BinaryProtocolAcceleratedFactory.new)
      data = serializer.serialize(SpecNamespace::Hello.new(:greeting => "'Ello guv'nor!"))
      expect(data).to eq("\x0B\x00\x01\x00\x00\x00\x0E'Ello guv'nor!\x00")
    end

    it "should serialize structs to the given protocol" do
      transport = double("transport")
      protocol = Thrift::BaseProtocol.new(transport)
      expect(protocol).to receive(:write_struct_begin).with("SpecNamespace::Hello")
      expect(protocol).to receive(:write_field_begin).with("greeting", Thrift::Types::STRING, 1)
      expect(protocol).to receive(:write_string).with("Good day")
      expect(protocol).to receive(:write_field_end)
      expect(protocol).to receive(:write_field_stop)
      expect(protocol).to receive(:write_struct_end)
      expect(transport).to receive(:flush)
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

    it "keeps ordinary protocol output byte-for-byte stable" do
      value = SpecNamespace::Hello.new(:greeting => "Good day")
      expected = {
        Thrift::BinaryProtocolFactory => "\v\x00\x01\x00\x00\x00\bGood day\x00".b,
        Thrift::CompactProtocolFactory => "\x18\bGood day\x00".b,
        Thrift::JsonProtocolFactory => "{\"1\":{\"str\":\"Good day\"}}"
      }

      expected.each do |factory_class, bytes|
        expect(Thrift::Serializer.new(factory_class.new).serialize(value)).to eq(bytes)
      end
    end

    it "finalizes and round-trips framed transport output" do
      value = SpecNamespace::Hello.new(:greeting => "Good day")
      factory = SerializerFixtures::FramedBinaryProtocolFactory.new

      data = Thrift::Serializer.new(factory).serialize(value)

      expect(data.unpack1("N")).to eq(data.bytesize - 4)
      decoded = SpecNamespace::Hello.new
      decoded.read(factory.get_protocol(Thrift::MemoryBufferTransport.new(data)))
      expect(decoded).to eq(value)
    end

    [
      Thrift::HeaderSubprotocolID::BINARY,
      Thrift::HeaderSubprotocolID::COMPACT
    ].each do |protocol_id|
      it "finalizes and round-trips Header protocol #{protocol_id}" do
        value = SpecNamespace::NonblockingService::Shutdown_args.new
        message = SerializerFixtures::HeaderMessage.new(value, 42)
        factory = Thrift::HeaderProtocolFactory.new(nil, protocol_id)

        data = Thrift::Serializer.new(factory).serialize(message)

        expect(data).not_to be_empty
        reader = factory.get_protocol(Thrift::MemoryBufferTransport.new(data))
        expect(reader.read_message_begin).to eq(["serialize", Thrift::MessageTypes::CALL, 42])
        expect(reader.get_headers).to eq("request-id" => "abc")
        decoded = SpecNamespace::NonblockingService::Shutdown_args.new
        decoded.read(reader)
        reader.read_message_end
        expect(decoded).to eq(value)
      end
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

    it "resets absent struct fields and restores defaults before reuse" do
      target = SpecNamespace::Foo.new
      first_payload = binary_payload do |protocol|
        protocol.write_field_begin("simple", Thrift::Types::I32, 1)
        protocol.write_i32(99)
        protocol.write_field_end
        protocol.write_field_begin("opt_string", Thrift::Types::STRING, 7)
        protocol.write_string("old")
        protocol.write_field_end
      end
      deserializer = Thrift::Deserializer.new

      deserializer.deserialize(target, first_payload)
      expect(target.simple).to eq(99)
      expect(target.opt_string).to eq("old")
      target.ints << 99
      target.complex = {1 => {"old" => 1.0}}

      mismatched_payload = binary_payload do |protocol|
        protocol.write_field_begin("simple", Thrift::Types::STRING, 1)
        protocol.write_string("ignored")
        protocol.write_field_end
      end
      deserializer.deserialize(target, mismatched_payload)

      expect(target.simple).to eq(53)
      expect(target.ints).to eq([1, 2, 2, 3])
      expect(target.complex).to be_nil
      expect(target.opt_string).to be_nil
    end

    it "does not retain previous struct state when reading fails" do
      target = SpecNamespace::Foo.new(:simple => 99, :opt_string => "old")
      payload = binary_payload(finish: false) do |protocol, transport|
        protocol.write_field_begin("opt_string", Thrift::Types::STRING, 7)
        transport.write([5].pack("N"))
      end

      expect {
        Thrift::Deserializer.new.deserialize(target, payload)
      }.to raise_error(EOFError)
      expect(target.simple).to eq(53)
      expect(target.opt_string).to be_nil
    end

    it "resets fields even when an accessor has the same name as the reset operation" do
      target = DeserializerResetFixtures::ResetFieldsStruct.new(:reset_fields => "old")

      Thrift::Deserializer.new.deserialize(target, binary_payload)

      expect(target.reset_fields).to be_nil
    end

    it "does not deserialize into a frozen struct" do
      target = DeserializerResetFixtures::ResetFieldsStruct.new.freeze
      payload = binary_payload do |protocol|
        protocol.write_field_begin("reset_fields", Thrift::Types::STRING, 1)
        protocol.write_string("new")
        protocol.write_field_end
      end

      expect {
        Thrift::Deserializer.new.deserialize(target, payload)
      }.to raise_error(FrozenError)
    end

    it "does not satisfy required-field validation with a value from the previous read" do
      target = DeserializerResetFixtures::RequiredStruct.new(:required_value => "old")

      expect {
        Thrift::Deserializer.new.deserialize(target, binary_payload)
      }.to raise_error(Thrift::ProtocolException, "Required field required_value is unset!")
      expect(target.required_value).to be_nil
    end

    it "does not retain a union variant when the next value has an unknown field" do
      target = SpecNamespace::TestUnion.new(:string_field, "old")
      payload = binary_payload do |protocol|
        protocol.write_field_begin("future", Thrift::Types::I32, 99)
        protocol.write_i32(1)
        protocol.write_field_end
      end

      expect {
        Thrift::Deserializer.new.deserialize(target, payload)
      }.to raise_error(Thrift::ProtocolException, "Union fields are not set.")
      expect(target.get_set_field).to be_nil
      expect(target.get_value).to be_nil
    end

    it "resets a union when an accessor has the same name as the reset operation" do
      target = DeserializerResetFixtures::ResetFieldsUnion.new(:reset_fields, "old")
      payload = binary_payload do |protocol|
        protocol.write_field_begin("future", Thrift::Types::I32, 99)
        protocol.write_i32(1)
        protocol.write_field_end
      end

      expect {
        Thrift::Deserializer.new.deserialize(target, payload)
      }.to raise_error(Thrift::ProtocolException, "Union fields are not set.")
      expect(target.get_set_field).to be_nil
      expect(target.get_value).to be_nil
    end

    it "does not deserialize into a frozen union" do
      target = DeserializerResetFixtures::ResetFieldsUnion.new.freeze
      payload = binary_payload do |protocol|
        protocol.write_field_begin("future", Thrift::Types::I32, 99)
        protocol.write_i32(1)
        protocol.write_field_end
      end

      expect {
        Thrift::Deserializer.new.deserialize(target, payload)
      }.to raise_error(FrozenError)
    end

    it "does not retain the previous union variant when reading fails" do
      target = SpecNamespace::TestUnion.new(:string_field, "old")
      payload = binary_payload(finish: false) do |protocol, transport|
        protocol.write_field_begin("string_field", Thrift::Types::STRING, 1)
        transport.write([5].pack("N"))
      end

      expect {
        Thrift::Deserializer.new.deserialize(target, payload)
      }.to raise_error(EOFError)
      expect(target.get_set_field).to be_nil
      expect(target.get_value).to be_nil
    end

    def binary_payload(finish: true)
      transport = Thrift::MemoryBufferTransport.new
      protocol = Thrift::BinaryProtocol.new(transport)
      protocol.write_struct_begin("value")
      yield protocol, transport if block_given?
      if finish
        protocol.write_field_stop
        protocol.write_struct_end
      end
      transport.read(transport.available)
    end
  end
end
