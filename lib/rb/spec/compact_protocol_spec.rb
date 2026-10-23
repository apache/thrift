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
    i64: [-(2**63), (2**63) - 1],
  }

  INTEGER_MINIMUM_ENCODINGS = {
    i32: [0xff, 0xff, 0xff, 0xff, 0x0f],
    i64: [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01],
  }

  INTEGER_VARINT_ENCODINGS = {
    0 => [0x00],
    -1 => [0x01],
    1 => [0x02],
    63 => [0x7e],
    64 => [0x80, 0x01],
    -64 => [0x7f],
    -65 => [0x81, 0x01],
    150 => [0xac, 0x02],
    -150 => [0xab, 0x02],
    8191 => [0xfe, 0x7f],
    8192 => [0x80, 0x80, 0x01],
    -8192 => [0xff, 0x7f],
    -8193 => [0x81, 0x80, 0x01],
    -32_768 => [0xff, 0xff, 0x03],
    32_767 => [0xfe, 0xff, 0x03],
    1_048_575 => [0xfe, 0xff, 0x7f],
    1_048_576 => [0x80, 0x80, 0x80, 0x01],
    -1_048_576 => [0xff, 0xff, 0x7f],
    -1_048_577 => [0x81, 0x80, 0x80, 0x01],
    134_217_727 => [0xfe, 0xff, 0xff, 0x7f],
    134_217_728 => [0x80, 0x80, 0x80, 0x80, 0x01],
    -134_217_728 => [0xff, 0xff, 0xff, 0x7f],
    -134_217_729 => [0x81, 0x80, 0x80, 0x80, 0x01],
    -2_147_483_648 => [0xff, 0xff, 0xff, 0xff, 0x0f],
    2_147_483_647 => [0xfe, 0xff, 0xff, 0xff, 0x0f],
    17_179_869_183 => [0xfe, 0xff, 0xff, 0xff, 0x7f],
    17_179_869_184 => [0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    -17_179_869_184 => [0xff, 0xff, 0xff, 0xff, 0x7f],
    -17_179_869_185 => [0x81, 0x80, 0x80, 0x80, 0x80, 0x01],
    2_199_023_255_551 => [0xfe, 0xff, 0xff, 0xff, 0xff, 0x7f],
    2_199_023_255_552 => [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    -2_199_023_255_552 => [0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],
    -2_199_023_255_553 => [0x81, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    281_474_976_710_655 => [0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],
    281_474_976_710_656 => [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    -281_474_976_710_656 => [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],
    -281_474_976_710_657 => [0x81, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    36_028_797_018_963_967 => [0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],
    36_028_797_018_963_968 => [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    -36_028_797_018_963_968 => [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],
    -36_028_797_018_963_969 => [0x81, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    4_611_686_018_427_387_903 => [0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],
    4_611_686_018_427_387_904 => [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    -4_611_686_018_427_387_904 => [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],
    -4_611_686_018_427_387_905 => [0x81, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01],
    -9_223_372_036_854_775_808 => [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01],
    9_223_372_036_854_775_807 => [0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01],
  }

  VARINT32_SIZE_ENCODINGS = {
    0 => [0x00],
    15 => [0x0f],
    127 => [0x7f],
    128 => [0x80, 0x01],
    150 => [0x96, 0x01],
    16_383 => [0xff, 0x7f],
    16_384 => [0x80, 0x80, 0x01],
    (2**21) - 1 => [0xff, 0xff, 0x7f],
    2**21 => [0x80, 0x80, 0x80, 0x01],
    (2**28) - 1 => [0xff, 0xff, 0xff, 0x7f],
    2**28 => [0x80, 0x80, 0x80, 0x80, 0x01],
    (2**31) - 1 => [0xff, 0xff, 0xff, 0xff, 0x07],
    2**31 => [0x80, 0x80, 0x80, 0x80, 0x08],
    (2**32) - 1 => [0xff, 0xff, 0xff, 0xff, 0x0f],
  }

  describe "varint writes" do
    let(:writes) { [] }
    let(:transport) { double("transport", message_boundaries?: false) }
    let(:protocol) { described_class.new(transport) }

    before do
      allow(transport).to receive(:write) do |buffer|
        writes << buffer
        nil
      end
    end

    INTEGER_BOUNDARY_TESTS.slice(:i16, :i32, :i64).each do |type, (minimum, maximum)|
      it "writes #{type} extrema and every width transition as one binary chunk" do
        examples = INTEGER_VARINT_ENCODINGS.select { |value, _| value.between?(minimum, maximum) }

        examples.each do |value, expected_bytes|
          writes.clear
          aggregate_failures("#{type} #{value}") do
            protocol.public_send("write_#{type}", value)
            expect(writes.map(&:bytes)).to eq([expected_bytes])
            expect(writes.first.encoding).to eq(Encoding::BINARY)
          end
        end
      end
    end

    it "keeps sequence ids unsigned on the wire and separate from envelope chunks" do
      {
        0 => [0x00],
        127 => [0x7f],
        128 => [0x80, 0x01],
        150 => [0x96, 0x01],
        16_383 => [0xff, 0x7f],
        16_384 => [0x80, 0x80, 0x01],
        (2**21) - 1 => [0xff, 0xff, 0x7f],
        2**21 => [0x80, 0x80, 0x80, 0x01],
        (2**28) - 1 => [0xff, 0xff, 0xff, 0x7f],
        2**28 => [0x80, 0x80, 0x80, 0x80, 0x01],
        (2**31) - 1 => [0xff, 0xff, 0xff, 0xff, 0x07],
        -2**31 => [0x80, 0x80, 0x80, 0x80, 0x08],
        -1 => [0xff, 0xff, 0xff, 0xff, 0x0f],
      }.each do |sequence_id, expected_bytes|
        writes.clear
        protocol.write_message_begin("x", Thrift::MessageTypes::CALL, sequence_id)
        expected = [[0x82], [0x21], expected_bytes, [1], [0x78]]
        expect(writes.map(&:bytes)).to eq(expected)
      end
    end

    it "batches collection sizes while preserving their type headers" do
      [
        15, 127, 128, 150, 16_383, 16_384, (2**21) - 1, 2**21, (2**28) - 1, 2**28, (2**31) - 1,
      ].each do |size|
        expected_bytes = VARINT32_SIZE_ENCODINGS.fetch(size)
        [:write_list_begin, :write_set_begin].each do |method|
          writes.clear
          protocol.public_send(method, Thrift::Types::I32, size)
          expect(writes.map(&:bytes)).to eq([[0xf5], expected_bytes])
        end
        writes.clear
        protocol.write_map_begin(Thrift::Types::I32, Thrift::Types::I64, size)
        expect(writes.map(&:bytes)).to eq([expected_bytes, [0x56]])
      end
    end

    it "batches non-delta field ids after their type header" do
      {
        -32_768 => [0xff, 0xff, 0x03],
        32_767 => [0xfe, 0xff, 0x03],
      }.each do |field_id, expected_bytes|
        writes.clear
        protocol.write_struct_begin("Value")
        protocol.write_field_begin("value", Thrift::Types::I32, field_id)
        expect(writes.map(&:bytes)).to eq([[0x05], expected_bytes])
        protocol.write_struct_end
      end
    end

    it "batches binary lengths while keeping the payload in its own write" do
      [0, 127, 128, 150, 16_383, 16_384].each do |size|
        writes.clear
        payload = "x".b * size
        protocol.write_binary(payload)
        expect(writes).to eq([VARINT32_SIZE_ENCODINGS.fetch(size).pack("C*"), payload])
      end
    end
  end

  TESTS = {
    byte: (-127..127).to_a,
    i16: (0..14).map { |shift| [1 << shift, -(1 << shift)] }.flatten.sort,
    i32: (0..30).map { |shift| [1 << shift, -(1 << shift)] }.flatten.sort,
    i64: (0..62).map { |shift| [1 << shift, -(1 << shift)] }.flatten.sort,
    string: ["", "1", "short", "fourteen123456", "fifteen12345678", "unicode characters: \u20AC \u20AD", "1" * 127, "1" * 3000],
    binary: ["", "\001", "\001" * 5, "\001" * 14, "\001" * 15, "\001" * 127, "\001" * 3000],
    double: [0.0, 1.0, -1.0, 1.1, -1.1, 10000000.1, 1.0 / 0.0, -1.0 / 0.0],
    bool: [true, false],
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
        _, type, id = proto.read_field_begin
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

      expect { proto.send(writer(primitive_type), 1.5) }.to raise_error(TypeError, "integer argument expected") do |error|
        expect(error).to be_instance_of(TypeError)
      end
      expect(trans.available).to eq(0)
    end
  end

  it "rejects nil fixed-width values consistently without writing" do
    INTEGER_BOUNDARY_TESTS.each_key do |primitive_type|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)

      expect { proto.send(writer(primitive_type), nil) }.to raise_error(TypeError, "integer argument expected") do |error|
        expect(error).to be_instance_of(TypeError)
      end
      expect(trans.available).to eq(0)
    end
  end

  it "validates field ids before changing protocol state or wire bytes" do
    [-2**15 - 1, 2**15, 1.5, nil].each do |field_id|
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)
      proto.write_struct_begin("Value")

      expected_error = field_id.is_a?(Integer) ? RangeError : TypeError
      expect {
        proto.write_field_begin("value", Thrift::Types::I32, field_id)
      }.to raise_error(expected_error) do |error|
        expect(error).to be_instance_of(expected_error)
      end
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
      expected_error = sequence_id.is_a?(Integer) ? RangeError : TypeError

      expect {
        proto.write_message_begin("value", Thrift::MessageTypes::CALL, sequence_id)
      }.to raise_error(expected_error) do |error|
        expect(error).to be_instance_of(expected_error)
      end
      expect(trans.available).to eq(0)
    end
  end

  it "writes message types with their exact envelope bytes" do
    {
      Thrift::MessageTypes::CALL => 0x21,
      Thrift::MessageTypes::REPLY => 0x41,
      Thrift::MessageTypes::EXCEPTION => 0x61,
      Thrift::MessageTypes::ONEWAY => 0x81,
      140 => 0x81,
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
      [:write_set_begin, [Thrift::Types::I16]],
    ]

    writers.each do |method, arguments|
      [-1, 2**31, 1.5, nil].each do |size|
        trans = Thrift::MemoryBufferTransport.new
        proto = Thrift::CompactProtocol.new(trans)
        expected_error = size.is_a?(Integer) ? RangeError : TypeError

        expect { proto.public_send(method, *arguments, size) }.to raise_error(expected_error) do |error|
          expect(error).to be_instance_of(expected_error)
        end
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
      Object.new => Thrift::CompactProtocol::CompactTypes::BOOLEAN_TRUE,
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
      [0x82, 0x00] => "Expected version 1 but got 0",
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
      read_list_begin: [0x1e],
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
      read_set_begin: [0xf5],
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
      write_list_begin: [99, 1],
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

  it "should narrow oversized varints to their declared integer widths" do
    aggregate_failures do
      {
        read_i16: [[0x80, 0x80, 0x04], -(2**15)],
        read_i64: [[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f], -(2**63)],
      }.each do |reader_method, (bytes, expected)|
        trans = Thrift::MemoryBufferTransport.new(bytes.pack("C*"))
        proto = Thrift::CompactProtocol.new(trans)

        expect(proto.public_send(reader_method)).to eq(expected)
      end
    end
  end

  it "should preserve fixed-width double edge byte patterns" do
    [
      [0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00],
      [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80],
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
    proto = Thrift::CompactProtocol.new(trans, max_string_size: nil)
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
        field2: "string2",
      ),
      field3: 3,
    )
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

  describe "binary payload storage" do
    [false, true].each do |frozen|
      [Encoding::BINARY, Encoding::UTF_8, Encoding::ISO_8859_1].each do |encoding|
        [:memory, :framed].each do |kind|
          it "round trips #{frozen ? "frozen" : "mutable"} #{encoding} input through #{kind}" do
            input = ("a\x00\xff".b * 1024).force_encoding(encoding)
            expected = input.b
            input.freeze if frozen
            memory = Thrift::MemoryBufferTransport.new
            transport = kind == :memory ? memory : Thrift::FramedTransport.new(memory)
            protocol = Thrift::CompactProtocol.new(transport)

            protocol.write_binary(input)
            transport.flush
            input.replace("changed") unless frozen
            expect(protocol.read_binary).to eq(expected)
            expect(input.encoding).to eq(encoding) if frozen
          end
        end
      end
    end

    context "with native callback ownership" do
      before do
        skip "native Compact ownership" unless Thrift::CompactProtocol.instance_method(:write_binary).source_location.nil?
      end

      [false, true].each do |frozen|
        [Encoding::BINARY, Encoding::UTF_8, Encoding::ISO_8859_1].each do |encoding|
          it "normalizes a separate #{frozen ? "frozen" : "mutable"} #{encoding} callback argument" do
            input = ("a\x00\xff".b * 1024).force_encoding(encoding)
            expected = input.b
            input.freeze if frozen
            writes = []
            transport = Thrift::BaseTransport.new
            transport.define_singleton_method(:write) { |bytes| writes << bytes }
            Thrift::CompactProtocol.new(transport).write_binary(input)

            expect(writes.last).to eq(expected)
            expect(writes.last.encoding).to eq(Encoding::BINARY)
            expect(writes.last).not_to equal(input)
            expect(writes.last).not_to be_frozen
            expect(input.encoding).to eq(frozen ? encoding : Encoding::BINARY)
          end
        end
      end

      [false, true].each do |frozen|
        it "isolates a #{frozen ? "frozen" : "mutable"} input from a mutating callback" do
          input = "payload".b * 1024
          original = input.dup
          input.freeze if frozen
          writes = []
          transport = Thrift::BaseTransport.new
          transport.define_singleton_method(:write) do |bytes|
            writes << bytes
            bytes.setbyte(0, 0)
          end

          expect(Thrift::CompactProtocol.new(transport).write_binary(input)).to be_nil
          expect(input).to eq(original)
          expect(writes.last).not_to equal(input)
          expect(writes.last).not_to be_frozen
          expect(writes.last.encoding).to eq(Encoding::BINARY)
        end
      end

      it "keeps retained payloads independent of later source and callback mutations" do
        writes = []
        transport = Thrift::BaseTransport.new
        transport.define_singleton_method(:write) { |bytes| writes << bytes }
        protocol = Thrift::CompactProtocol.new(transport)
        input = "a".b * 4096
        2.times { protocol.write_binary(input) }
        input.replace("changed")
        GC.start
        GC.verify_compaction_references(double_heap: true, toward: :empty) if GC.respond_to?(:verify_compaction_references)

        expect(writes[1]).to eq("a".b * 4096)
        writes[1].setbyte(0, 98)
        expect(writes[3]).to eq("a".b * 4096)
        expect(input).to eq("changed")
      end

      it "passes a plain String without invoking input subclass copy hooks" do
        klass = Class.new(String) do
          def initialize_copy(other)
            raise "unexpected copy hook"
          end
        end
        input = klass.new("a".b * 4096).freeze
        writes = []
        transport = Thrift::BaseTransport.new
        transport.define_singleton_method(:write) { |bytes| writes << bytes }
        Thrift::CompactProtocol.new(transport).write_binary(input)

        expect(writes.last).to be_instance_of(String)
        expect(writes.last).to eq(input)
      end

      it "honors MemoryBuffer write overrides" do
        transport = Thrift::MemoryBufferTransport.new
        writes = []
        transport.define_singleton_method(:write) do |bytes|
          writes << bytes
          super(bytes)
        end
        protocol = Thrift::CompactProtocol.new(transport)
        protocol.write_binary("payload".b.freeze)

        expect(writes.map(&:bytesize)).to eq([1, 7])
        expect(protocol.read_binary).to eq("payload")
      end

      it "honors MemoryBuffer subclass write overrides" do
        klass = Class.new(Thrift::MemoryBufferTransport) do
          attr_reader :writes

          def write(bytes)
            (@writes ||= []) << bytes
            super
          end
        end
        transport = klass.new
        protocol = Thrift::CompactProtocol.new(transport)
        protocol.write_binary("a".b * 4096)

        expect(transport.writes.map(&:bytesize)).to eq([2, 4096])
        expect(protocol.read_binary).to eq("a".b * 4096)
      end

      it "passes a mutable empty payload after its length prefix" do
        writes = []
        transport = Thrift::BaseTransport.new
        transport.define_singleton_method(:write) { |bytes| writes << bytes }
        Thrift::CompactProtocol.new(transport).write_binary("".b.freeze)
        expect(writes).to eq(["\x00".b, "".b])
        expect(writes.last).not_to be_frozen
      end

      it "keeps write_string input separate from a mutating callback" do
        input = ("\u20ac" * 1024).freeze
        writes = []
        transport = Thrift::BaseTransport.new
        transport.define_singleton_method(:write) do |bytes|
          writes << bytes
          bytes.clear
        end
        Thrift::CompactProtocol.new(transport).write_string(input)
        expect(input).to eq("\u20ac" * 1024)
        expect(input.encoding).to eq(Encoding::UTF_8)
        expect(writes.last.encoding).to eq(Encoding::BINARY)
      end

      it "preserves the declared length when the header callback appends to the source" do
        input = "abc".b
        writes = []
        transport = Thrift::BaseTransport.new
        transport.define_singleton_method(:write) do |bytes|
          input << "d" if writes.empty?
          writes << bytes
        end
        Thrift::CompactProtocol.new(transport).write_binary(input)
        expect(writes).to eq(["\x03".b, "abc".b])
      end

      [false, true].each do |frozen|
        it "passes binary bytes when the header callback re-encodes #{frozen ? "and freezes" : "a mutable"} input" do
          input = "a".b * 4096
          expected = input.dup
          output = "".b
          writes = []
          transport = Thrift::BaseTransport.new
          transport.define_singleton_method(:write) do |bytes|
            if writes.empty?
              input.force_encoding(Encoding::UTF_16LE)
              input.freeze if frozen
            end
            writes << bytes
            output << bytes
          end

          Thrift::CompactProtocol.new(transport).write_binary(input)

          expect(writes.last.encoding).to eq(Encoding::BINARY)
          expect(writes.last).not_to equal(input)
          expect(writes.last).not_to be_frozen
          expect(input.encoding).to eq(Encoding::UTF_16LE)
          expect(Thrift::CompactProtocol.new(Thrift::MemoryBufferTransport.new(output)).read_binary).to eq(expected)
        end
      end

      it "propagates payload callback exceptions without changing the input" do
        input = "abc".b.freeze
        transport = Thrift::BaseTransport.new
        transport.define_singleton_method(:write) do |bytes|
          raise IOError, "payload rejected" if bytes.bytesize == 3
        end
        expect { Thrift::CompactProtocol.new(transport).write_binary(input) }.to raise_error(IOError, "payload rejected")
        expect(input).to eq("abc")
      end
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
    "write_#{sym}"
  end

  def reader(sym)
    "read_#{sym}"
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
