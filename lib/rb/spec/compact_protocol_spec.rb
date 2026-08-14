# encoding: UTF-8
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

describe Thrift::CompactProtocol do
  INTEGER_BOUNDARY_TESTS = {
    byte: [-(2**7), (2**7) - 1],
    i16: [-(2**15), (2**15) - 1],
    i32: [-(2**31), (2**31) - 1],
    i64: [-(2**63), (2**63) - 1]
  }

  INTEGER_MINIMUM_ENCODINGS = {
    i32: [0xff, 0xff, 0xff, 0xff, 0x0f],
    i64: [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01]
  }

  VARINT32_SIZE_ENCODINGS = {
    (2**31) - 1 => [0xff, 0xff, 0xff, 0xff, 0x07],
    2**31 => [0x80, 0x80, 0x80, 0x80, 0x08],
    (2**32) - 1 => [0xff, 0xff, 0xff, 0xff, 0x0f]
  }

  TESTS = {
    byte: (-127..127).to_a,
    i16: (0..14).map { |shift| [1 << shift, -(1 << shift)] }.flatten.sort,
    i32: (0..30).map { |shift| [1 << shift, -(1 << shift)] }.flatten.sort,
    i64: (0..62).map { |shift| [1 << shift, -(1 << shift)] }.flatten.sort,
    string: ["", "1", "short", "fourteen123456", "fifteen12345678", "unicode characters: \u20AC \u20AD", "1" * 127, "1" * 3000],
    binary: ["", "\001", "\001" * 5, "\001" * 14, "\001" * 15, "\001" * 127, "\001" * 3000],
    double: [0.0, 1.0, -1.0, 1.1, -1.1, 10000000.1, 1.0/0.0, -1.0/0.0],
    bool: [true, false]
  }

  it "should encode and decode naked primitives correctly" do
    TESTS.each_pair do |primitive_type, test_values|
      test_values.each do |value|
        # puts "testing #{value}" if primitive_type == :i64
        trans = Thrift::MemoryBufferTransport.new
        proto = Thrift::CompactProtocol.new(trans)

        proto.send(writer(primitive_type), value)
        # puts "buf: #{trans.inspect_buffer}" if primitive_type == :i64
        read_back = proto.send(reader(primitive_type))
        expect(read_back).to eq(value)
      end
    end
  end

  it "should skip strings as binary values" do
    trans = Thrift::MemoryBufferTransport.new
    proto = Thrift::CompactProtocol.new(trans)
    proto.write_binary("value")
    expect(proto).to receive(:read_binary).and_call_original
    expect(proto).not_to receive(:read_string)

    proto.skip(Thrift::Types::STRING)

    expect(trans.available).to eq(0)
  end

  it "should encode and decode primitives in fields correctly" do
    TESTS.each_pair do |primitive_type, test_values|
      final_primitive_type = primitive_type == :binary ? :string : primitive_type
      thrift_type = Thrift::Types.const_get(final_primitive_type.to_s.upcase)
      # puts primitive_type
      test_values.each do |value|
        trans = Thrift::MemoryBufferTransport.new
        proto = Thrift::CompactProtocol.new(trans)

        proto.write_field_begin(nil, thrift_type, 15)
        proto.send(writer(primitive_type), value)
        proto.write_field_end

        proto = Thrift::CompactProtocol.new(trans)
        name, type, id = proto.read_field_begin
        expect(type).to eq(thrift_type)
        expect(id).to eq(15)
        read_back = proto.send(reader(primitive_type))
        expect(read_back).to eq(value)
        proto.read_field_end
      end
    end
  end

  it "should round-trip signed integer boundaries correctly" do
    INTEGER_BOUNDARY_TESTS.each_pair do |primitive_type, test_values|
      test_values.each do |value|
        trans = Thrift::MemoryBufferTransport.new
        proto = Thrift::CompactProtocol.new(trans)

        proto.send(writer(primitive_type), value)
        expect(proto.send(reader(primitive_type))).to eq(value)
      end
    end
  end

  it "rejects fixed-width integers outside their declared ranges without writing" do
    INTEGER_BOUNDARY_TESTS.each do |primitive_type, (minimum, maximum)|
      [minimum - 1, maximum + 1].each do |value|
        trans = Thrift::MemoryBufferTransport.new
        proto = Thrift::CompactProtocol.new(trans)

        expect { proto.send(writer(primitive_type), value) }.to raise_error(RangeError)
        expect(trans.available).to eq(0)
      end
    end
  end

  it "rejects non-integer fixed-width values consistently without writing" do
    INTEGER_BOUNDARY_TESTS.each_key do |primitive_type|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)

      expect { proto.send(writer(primitive_type), 1.5) }.to raise_error(TypeError, "integer argument expected")
      expect(trans.available).to eq(0)
    end
  end

  it "rejects nil fixed-width values consistently without writing" do
    INTEGER_BOUNDARY_TESTS.each_key do |primitive_type|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)

      expect { proto.send(writer(primitive_type), nil) }.to raise_error(StandardError, "nil argument not allowed!")
      expect(trans.available).to eq(0)
    end
  end

  it "validates field ids before changing protocol state or wire bytes" do
    [-2**15 - 1, 2**15, 1.5, nil].each do |field_id|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)
      proto.write_struct_begin("Value")

      expected_error = field_id.nil? ? StandardError : field_id.is_a?(Integer) ? RangeError : TypeError
      expect {
        proto.write_field_begin("value", Thrift::Types::I32, field_id)
      }.to raise_error(expected_error)
      expect(trans.available).to eq(0)
    end

    [-2**15, (2**15) - 1].each do |field_id|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)
      proto.write_struct_begin("Value")
      expect { proto.write_field_begin("value", Thrift::Types::I32, field_id) }.not_to raise_error
    end
  end

  it "validates message sequence ids before writing the envelope" do
    [-2**31 - 1, 2**31, 1.5, nil].each do |sequence_id|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)
      expected_error = sequence_id.nil? ? StandardError : sequence_id.is_a?(Integer) ? RangeError : TypeError

      expect {
        proto.write_message_begin("value", Thrift::MessageTypes::CALL, sequence_id)
      }.to raise_error(expected_error)
      expect(trans.available).to eq(0)
    end
  end

  it "writes message types with their exact envelope bytes" do
    {
      Thrift::MessageTypes::CALL => 0x21,
      Thrift::MessageTypes::REPLY => 0x41,
      Thrift::MessageTypes::EXCEPTION => 0x61,
      Thrift::MessageTypes::ONEWAY => 0x81,
      140 => 0x81
    }.each do |message_type, version_and_type|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)

      proto.write_message_begin("", message_type, 0)

      expect(trans.read(trans.available).bytes).to eq([0x82, version_and_type, 0, 0])
    end
  end

  it "validates collection sizes before writing their headers" do
    writers = [
      [:write_map_begin, [Thrift::Types::STRING, Thrift::Types::I32]],
      [:write_list_begin, [Thrift::Types::I16]],
      [:write_set_begin, [Thrift::Types::I16]]
    ]

    writers.each do |method, arguments|
      [-1, 2**31, 1.5, nil].each do |size|
        trans = Thrift::MemoryBufferTransport.new
        proto = Thrift::CompactProtocol.new(trans)
        expected_error = size.nil? ? StandardError : size.is_a?(Integer) ? RangeError : TypeError

        expect { proto.public_send(method, *arguments, size) }.to raise_error(expected_error)
        expect(trans.available).to eq(0)
      end

      [0, (2**31) - 1].each do |size|
        trans = Thrift::MemoryBufferTransport.new
        proto = Thrift::CompactProtocol.new(trans)
        expect { proto.public_send(method, *arguments, size) }.not_to raise_error
      end
    end
  end

  it "does not append rejected integers after field and collection headers" do
    trans = Thrift::MemoryBufferTransport.new
    proto = Thrift::CompactProtocol.new(trans)
    proto.write_struct_begin("Value")
    proto.write_field_begin("value", Thrift::Types::I16, 1)
    field_header_size = trans.available
    expect { proto.write_i16(2**15) }.to raise_error(RangeError)
    expect(trans.available).to eq(field_header_size)

    proto.write_list_begin(Thrift::Types::I32, 1)
    list_header_size = trans.available
    expect { proto.write_i32(2**31) }.to raise_error(RangeError)
    expect(trans.available).to eq(list_header_size)
  end

  it "should encode signed integer minima with the canonical zigzag varint bytes" do
    INTEGER_MINIMUM_ENCODINGS.each_pair do |primitive_type, expected_bytes|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)

      proto.send(writer(primitive_type), INTEGER_BOUNDARY_TESTS.fetch(primitive_type).first)
      expect(trans.read(trans.available).bytes).to eq(expected_bytes)
    end
  end

  it "should use Ruby truthiness when writing bools" do
    {
      true => Thrift::CompactProtocol::CompactTypes::BOOLEAN_TRUE,
      false => Thrift::CompactProtocol::CompactTypes::BOOLEAN_FALSE,
      nil => Thrift::CompactProtocol::CompactTypes::BOOLEAN_FALSE,
      0 => Thrift::CompactProtocol::CompactTypes::BOOLEAN_TRUE,
      1 => Thrift::CompactProtocol::CompactTypes::BOOLEAN_TRUE,
      Object.new => Thrift::CompactProtocol::CompactTypes::BOOLEAN_TRUE
    }.each do |value, expected|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)

      proto.write_bool(value)

      expect(trans.read_byte).to eq(expected)
    end
  end

  it "should report malformed message headers consistently" do
    {
      [0x00] => "Expected protocol id -126 but got 0",
      [0x82, 0x00] => "Expected version 1 but got 0"
    }.each do |bytes, expected_message|
      trans = Thrift::MemoryBufferTransport.new(bytes.pack("C*"))
      proto = Thrift::CompactProtocol.new(trans)

      expect { proto.read_message_begin }.to raise_error(Thrift::ProtocolException, expected_message) do |error|
        expect(error.type).to eq(Thrift::ProtocolException::BAD_VERSION)
      end
    end
  end

  it "should reject unknown field and container types as invalid protocol data" do
    {
      read_field_begin: [0x1e],
      read_list_begin: [0x1e]
    }.each do |reader_method, bytes|
      trans = Thrift::MemoryBufferTransport.new(bytes.pack("C*"))
      proto = Thrift::CompactProtocol.new(trans)

      expect { proto.public_send(reader_method) }.to raise_error(Thrift::ProtocolException, "Unknown compact type: 14") do |error|
        expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
      end
    end
  end

  it "should accept container sizes within the signed 32-bit range" do
    bytes = [0xf5, *VARINT32_SIZE_ENCODINGS.fetch((2**31) - 1)]
    trans = Thrift::MemoryBufferTransport.new(bytes.pack("C*"))
    proto = Thrift::CompactProtocol.new(trans)

    expect(proto.read_list_begin).to eq([Thrift::Types::I32, (2**31) - 1])
  end

  it "should reject container sizes above the signed 32-bit range" do
    {
      read_map_begin: [0x55],
      read_list_begin: [0xf5],
      read_set_begin: [0xf5]
    }.each do |reader_method, container_header|
      [2**31, (2**32) - 1].each do |size|
        bytes = if reader_method == :read_map_begin
                  [*VARINT32_SIZE_ENCODINGS.fetch(size), *container_header]
                else
                  [*container_header, *VARINT32_SIZE_ENCODINGS.fetch(size)]
                end
        trans = Thrift::MemoryBufferTransport.new(bytes.pack("C*"))
        proto = Thrift::CompactProtocol.new(trans)

        expect { proto.public_send(reader_method) }.to raise_error(Thrift::ProtocolException, "Container size limit exceeded") do |error|
          expect(error.type).to eq(Thrift::ProtocolException::SIZE_LIMIT)
        end
      end
    end
  end

  it "should report the original unknown type when writing fields and containers" do
    {
      write_field_begin: [nil, 99, 1],
      write_list_begin: [99, 1]
    }.each do |writer_method, args|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)

      expect { proto.public_send(writer_method, *args) }.to raise_error(Thrift::ProtocolException, "Unknown compact type: 99") do |error|
        expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
      end
      expect(trans.available).to eq(0)
    end
  end

  it "should decode i32 minima from direct canonical zigzag bytes" do
    trans = Thrift::MemoryBufferTransport.new
    trans.write(INTEGER_MINIMUM_ENCODINGS[:i32].pack("C*"))

    proto = Thrift::CompactProtocol.new(trans)
    expect(proto.read_i32).to eq(INTEGER_BOUNDARY_TESTS[:i32].first)
  end

  it "should decode i64 minima from direct canonical zigzag bytes" do
    trans = Thrift::MemoryBufferTransport.new
    trans.write(INTEGER_MINIMUM_ENCODINGS[:i64].pack("C*"))

    proto = Thrift::CompactProtocol.new(trans)
    expect(proto.read_i64).to eq(INTEGER_BOUNDARY_TESTS[:i64].first)
  end

  it "should preserve fixed-width double edge byte patterns" do
    [
      [0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00],
      [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80]
    ].each do |bytes|
      trans = Thrift::MemoryBufferTransport.new(bytes.pack("C*"))
      proto = Thrift::CompactProtocol.new(trans)

      expect([proto.read_double].pack("G").reverse.bytes).to eq(bytes)
    end
  end

  it "should read binary values with multi-byte varint32 lengths" do
    payload = "x" * 128
    trans = Thrift::MemoryBufferTransport.new
    trans.write([0x80, 0x01].pack("C*"))
    trans.write(payload)

    proto = Thrift::CompactProtocol.new(trans)
    expect(proto.read_binary).to eq(payload)
  end

  it "should accept binary sizes through the signed 32-bit range" do
    trans = Thrift::MemoryBufferTransport.new(VARINT32_SIZE_ENCODINGS.fetch((2**31) - 1).pack("C*"))
    proto = Thrift::CompactProtocol.new(trans)
    expect(trans).to receive(:read_all).with((2**31) - 1).and_return("payload")

    expect(proto.read_binary).to eq("payload")
  end

  it "should reject binary sizes above the signed 32-bit range before reading the payload" do
    [2**31, (2**32) - 1].each do |size|
      trans = Thrift::MemoryBufferTransport.new(VARINT32_SIZE_ENCODINGS.fetch(size).pack("C*"))
      proto = Thrift::CompactProtocol.new(trans)
      expect(trans).not_to receive(:read_all)

      expect { proto.read_binary }.to raise_error(Thrift::ProtocolException, "Binary size limit exceeded") do |error|
        expect(error.type).to eq(Thrift::ProtocolException::SIZE_LIMIT)
      end
    end
  end

  it "should write a uuid" do
    trans = Thrift::MemoryBufferTransport.new
    proto = Thrift::CompactProtocol.new(trans)

    proto.write_uuid("00112233-4455-6677-8899-aabbccddeeff")
    a = trans.read(trans.available)
    expect(a.unpack("C*")).to eq([0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff])
  end

  it "should read a uuid" do
    trans = Thrift::MemoryBufferTransport.new
    proto = Thrift::CompactProtocol.new(trans)

    trans.write([0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff].pack("C*"))
    uuid = proto.read_uuid
    expect(uuid).to eq("00112233-4455-6677-8899-aabbccddeeff")
  end

  it "should error gracefully when trying to write an invalid uuid" do
    trans = Thrift::MemoryBufferTransport.new
    proto = Thrift::CompactProtocol.new(trans)
    expect { proto.write_uuid("invalid") }.to raise_error(Thrift::ProtocolException)
  end

  it "should encode and decode a monster struct correctly" do
    trans = Thrift::MemoryBufferTransport.new
    proto = Thrift::CompactProtocol.new(trans)

    struct = Thrift::Test::CompactProtoTestStruct.new
    # sets and maps don't hash well... not sure what to do here.
    struct.write(proto)

    struct2 = Thrift::Test::CompactProtoTestStruct.new
    struct2.read(proto)
    expect(struct2).to eq(struct)
  end

  it "should make method calls correctly" do
    client_out_trans = Thrift::MemoryBufferTransport.new
    client_out_proto = Thrift::CompactProtocol.new(client_out_trans)

    client_in_trans = Thrift::MemoryBufferTransport.new
    client_in_proto = Thrift::CompactProtocol.new(client_in_trans)

    processor = Thrift::Test::Srv::Processor.new(JankyHandler.new)

    client = Thrift::Test::Srv::Client.new(client_in_proto, client_out_proto)
    client.send_Janky(1)
    # puts client_out_trans.inspect_buffer
    processor.process(client_out_proto, client_in_proto)
    expect(client.recv_Janky).to eq(2)
  end

  it "should round-trip wrapped negative seqids in message headers" do
    trans = Thrift::MemoryBufferTransport.new
    writer = Thrift::CompactProtocol.new(trans)

    writer.write_message_begin("test", Thrift::MessageTypes::CALL, -2147483648)
    writer.write_message_end

    reader = Thrift::CompactProtocol.new(trans)
    name, type, seqid = reader.read_message_begin
    expect(name).to eq("test")
    expect(type).to eq(Thrift::MessageTypes::CALL)
    expect(seqid).to eq(-2147483648)
  end

  it "should deal with fields following fields that have non-delta ids" do
    brcp = Thrift::Test::BreaksRubyCompactProtocol.new(
      field1: "blah",
      field2: Thrift::Test::BigFieldIdStruct.new(
        field1: "string1",
        field2: "string2"),
      field3: 3)
    ser = Thrift::Serializer.new(Thrift::CompactProtocolFactory.new)
    bytes = ser.serialize(brcp)

    deser = Thrift::Deserializer.new(Thrift::CompactProtocolFactory.new)
    brcp2 = Thrift::Test::BreaksRubyCompactProtocol.new
    deser.deserialize(brcp2, bytes)
    expect(brcp2).to eq(brcp)
  end

  it "should deserialize an empty map to an empty hash" do
    struct = Thrift::Test::SingleMapTestStruct.new(i32_map: {})
    ser = Thrift::Serializer.new(Thrift::CompactProtocolFactory.new)
    bytes = ser.serialize(struct)

    deser = Thrift::Deserializer.new(Thrift::CompactProtocolFactory.new)
    struct2 = Thrift::Test::SingleMapTestStruct.new
    deser.deserialize(struct2, bytes)
    expect(struct).to eq(struct2)
  end

  it "should not resolve omitted types when writing an empty map" do
    trans = Thrift::MemoryBufferTransport.new
    proto = Thrift::CompactProtocol.new(trans)

    proto.write_map_begin(nil, nil, 0)

    expect(trans.read(trans.available)).to eq("\x00".b)
  end

  it "should provide a reasonable to_s" do
    trans = Thrift::MemoryBufferTransport.new
    expect(Thrift::CompactProtocol.new(trans).to_s).to eq("compact(memory)")
  end

  it "should write a frozen non-binary string without mutating the input" do
    trans = Thrift::MemoryBufferTransport.new
    prot = Thrift::CompactProtocol.new(trans)
    buffer = "abc \u20AC".encode("UTF-8").freeze

    prot.write_binary(buffer)

    expect(buffer.encoding).to eq(Encoding::UTF_8)
    expect(buffer).to be_frozen
    expect(trans.read(trans.available).unpack("C*")).to eq([0x07, 0x61, 0x62, 0x63, 0x20, 0xE2, 0x82, 0xAC])
  end

  it "should reject a varint with more than 10 continuation bytes" do
    trans = Thrift::MemoryBufferTransport.new(([0x80] * 11).pack("C*"))
    proto = Thrift::CompactProtocol.new(trans)
    expect { proto.read_i64 }.to raise_error(Thrift::ProtocolException) do |e|
      expect(e.type).to eq(Thrift::ProtocolException::INVALID_DATA)
    end
  end

  it "should accept a valid 10-byte varint" do
    trans = Thrift::MemoryBufferTransport.new((([0x80] * 9) + [0x01]).pack("C*"))
    proto = Thrift::CompactProtocol.new(trans)
    expect { proto.read_i64 }.not_to raise_error
  end

  it "should reject a 32-bit varint with more than 5 continuation bytes" do
    trans = Thrift::MemoryBufferTransport.new(([0x80] * 6).pack("C*"))
    proto = Thrift::CompactProtocol.new(trans)
    expect { proto.read_i32 }.to raise_error(Thrift::ProtocolException) do |e|
      expect(e.type).to eq(Thrift::ProtocolException::INVALID_DATA)
    end
  end

  it "should accept a valid 5-byte varint for i32" do
    trans = Thrift::MemoryBufferTransport.new((([0x80] * 4) + [0x0f]).pack("C*"))
    proto = Thrift::CompactProtocol.new(trans)
    expect { proto.read_i32 }.not_to raise_error
  end

  it "should reject 32-bit varints with overflowing fifth-byte payload bits" do
    [0x10, 0x7f].each do |terminal_byte|
      trans = Thrift::MemoryBufferTransport.new((([0x80] * 4) + [terminal_byte]).pack("C*"))
      proto = Thrift::CompactProtocol.new(trans)

      expect { proto.read_i32 }.to raise_error(Thrift::ProtocolException, "Variable-length int overflows uint32.") do |error|
        expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
      end
    end
  end

  class JankyHandler
    def Janky(i32arg)
      i32arg * 2
    end
  end

  def writer(sym)
    "write_#{sym.to_s}"
  end

  def reader(sym)
    "read_#{sym.to_s}"
  end
end

describe Thrift::CompactProtocolFactory do
  it "should create a CompactProtocol" do
    transport = Thrift::MemoryBufferTransport.new
    expect(Thrift::CompactProtocolFactory.new.get_protocol(transport)).to be_instance_of(Thrift::CompactProtocol)
  end

  it "should provide a reasonable to_s" do
    expect(Thrift::CompactProtocolFactory.new.to_s).to eq("compact")
  end
end
