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

describe "JSON input byte ownership" do
  class FragmentedJsonInput < Thrift::BaseTransport
    attr_reader :requested

    def initialize(parts)
      @parts = parts.map(&:b)
      @requested = []
    end

    def read(size)
      @requested << size
      @parts.shift while @parts.first == ""
      raise EOFError, "fragmented input exhausted" if @parts.empty?

      @parts.first.slice!(0, size)
    end
  end

  it "reads Unicode, escapes, base64 and numbers across every fragment boundary" do
    wire = <<~'JSON'.chomp.b
      ["str",3,"raw € 😀\u20ac\uD83D\uDE00\\\"\/\b\f\n\r\t","QUJDRA",-12.5e+2]
    JSON
    (1...wire.bytesize).each do |split|
      input = FragmentedJsonInput.new([wire.byteslice(0, split), wire.byteslice(split..-1)])
      protocol = Thrift::JsonProtocol.new(Thrift::BufferedTransport.new(input))
      expect(protocol.read_list_begin).to eq([Thrift::Types::STRING, 3])
      expect(protocol.read_string).to eq("raw € 😀€😀\\\"/\b\f\n\r\t")
      expect(protocol.read_binary).to eq("ABCD")
      expect(protocol.read_double).to eq(-1250.0)
      protocol.read_list_end
    end
  end

  it "leaves the following value in the transport and owns at most one peeked byte" do
    trans = Thrift::MemoryBufferTransport.new('"first"123,"second"')
    protocol = Thrift::JsonProtocol.new(trans)
    expect(protocol.read_string).to eq("first")
    expect(trans.available).to eq('123,"second"'.bytesize)
    expect(protocol.read_i32).to eq(123)
    expect(trans.read(1)).to eq('"') # Only the comma belongs to the reader.
    reader = protocol.instance_variable_get(:@reader)
    expect(reader.peek).to eq(",")
    expect(reader.read).to eq(",")
  end

  it "falls back to read when the transport has no byte API" do
    klass = Class.new(Thrift::MemoryBufferTransport) { undef_method :read_byte }
    value = "v" * 64
    expect(Thrift::JsonProtocol.new(klass.new('"' + value + '"')).read_string).to eq(value)
  end

  it "uses the byte API on a duck-typed transport" do
    value = "v" * 64
    input = Thrift::MemoryBufferTransport.new('"' + value + '"')
    transport = double("Transport")
    allow(transport).to receive(:read).with(1) { input.read(1) }
    expect(transport).to receive(:read_byte).at_least(:once) { input.read_byte }

    expect(Thrift::JsonProtocol.new(transport).read_string).to eq(value)
    expect(input.available).to eq(0)
  end

  it "preserves Unicode decoding hooks" do
    protocol_class = Class.new(Thrift::JsonProtocol) do
      def read_json_escape_char
        super.upcase
      end
    end
    trans = Thrift::MemoryBufferTransport.new('"' + ("v" * 64) + 'a\\u0062"')
    expect(protocol_class.new(trans).read_string).to eq(("v" * 64) + "aB")
  end

  it "keeps tiny strings on the read path without inspecting byte-read eligibility" do
    protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new('"""a"'))
    reader = protocol.instance_variable_get(:@reader)
    expect(reader).not_to receive(:byte_reads?)
    expect(protocol.read_string).to eq("")
    expect(protocol.read_string).to eq("a")
  end

  it "selects byte reads once when Unicode escapes cross the small-string threshold" do
    wire = '"' + ('\\u20ac' * 6) + ("a" * 1024) + '"'
    protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new(wire))
    reader = protocol.instance_variable_get(:@reader)
    expect(reader).to receive(:byte_reads?).once.and_call_original
    expect(reader).to receive(:read_byte).at_least(:once).and_call_original
    expect(protocol.read_string).to eq(("€" * 6) + ("a" * 1024))
  end

  [Thrift::MemoryBufferTransport, Thrift::BufferedTransport].each do |klass|
    it "uses the byte API when #{klass} customizes it" do
      reads = 0
      transport_class = Class.new(klass) do
        define_method(:read_byte) do
          reads += 1
          super()
        end
      end
      value = "a" * 64
      wire = '"' + value + '"'
      trans = if klass == Thrift::BufferedTransport
        transport_class.new(FragmentedJsonInput.new([wire]))
      else
        transport_class.new(wire)
      end
      expect(Thrift::JsonProtocol.new(trans).read_string).to eq(value)
      expect(reads).to be > 0
    end
  end

  it "supports duck-typed readers" do
    value = "v" * 64
    trans = Thrift::MemoryBufferTransport.new('"' + value + '"')
    reader = Object.new
    reader.define_singleton_method(:read) { trans.read(1) }
    protocol = Thrift::JsonProtocol.new(trans)
    protocol.instance_variable_set(:@reader, reader)
    expect(protocol.read_string).to eq(value)
  end

  it "preserves invalid UTF-8 and Unicode errors across fragments" do
    ["\"\xC3\x28\"".b, '"\\uD83D\\u0041"', '"\\uDE00"'].each do |wire|
      (1...wire.bytesize).each do |split|
        input = FragmentedJsonInput.new([wire.byteslice(0, split), wire.byteslice(split..-1)])
        protocol = Thrift::JsonProtocol.new(Thrift::BufferedTransport.new(input))
        expect { protocol.read_string }.to raise_error(Thrift::ProtocolException) do |error|
          expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
        end
      end
    end
  end

  it "retains mutable independent read and peek results" do
    reader = Thrift::LookaheadReader.new(Thrift::MemoryBufferTransport.new("ab"))
    first = reader.peek
    expect(reader.read).to equal(first)
    second = reader.read
    second.replace("z")
    expect(first).to eq("a")
  end

  it "does not consume the next frame when a string ends at a frame boundary" do
    value = "a" * 64
    first = '"' + value + '"'
    wire = [first.bytesize].pack("N") + first + [3].pack("N") + '"b"'
    input = Thrift::MemoryBufferTransport.new(wire)
    protocol = Thrift::JsonProtocol.new(Thrift::FramedTransport.new(input))
    expect(protocol.read_string).to eq(value)
    expect(input.available).to eq(7)
    expect(protocol.read_string).to eq("b")
    expect(input.available).to eq(0)

    value = ('"' + ("a" * 64) + '€\\uD83D\\uDE00"').b
    wire = value.bytes.map { |byte| [1, byte].pack("NC") }.join
    protocol = Thrift::JsonProtocol.new(Thrift::FramedTransport.new(Thrift::MemoryBufferTransport.new(wire)))
    expect(protocol.read_string).to eq(("a" * 64) + "€😀")
  end

  it "preserves truncated input errors in plain and Unicode strings" do
    ['"abc', '"abc\\'].each do |wire|
      protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new(wire))
      expect { protocol.read_string }.to raise_error(EOFError, /Not enough bytes remain/)
    end
    ['"\\u123', '"\\uD83D\\uDE0'].each do |wire|
      protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new(wire))
      expect { protocol.read_string }.to raise_error(Thrift::ProtocolException, "Incomplete Unicode escape")
    end
  end
end

describe "JsonProtocol" do
  describe Thrift::JsonProtocol do
    before(:each) do
      @trans = Thrift::MemoryBufferTransport.new
      @prot = Thrift::JsonProtocol.new(@trans)
    end

    it "should write json escaped char" do
      @prot.write_json_escape_char("\n")
      expect(@trans.read(@trans.available)).to eq('\u000a')

      @prot.write_json_escape_char(" ")
      expect(@trans.read(@trans.available)).to eq('\u0020')
    end

    it "should write json char" do
      @prot.write_json_char("\n")
      expect(@trans.read(@trans.available)).to eq('\\n')

      @prot.write_json_char(" ")
      expect(@trans.read(@trans.available)).to eq(" ")

      @prot.write_json_char("\\")
      expect(@trans.read(@trans.available)).to eq("\\\\")

      @prot.write_json_char("@")
      expect(@trans.read(@trans.available)).to eq("@")
    end

    it "should write json string" do
      @prot.write_json_string("this is a \\ json\nstring")
      expect(@trans.read(@trans.available)).to eq("\"this is a \\\\ json\\nstring\"")
    end

    it "writes every ASCII character with the existing JSON escapes" do
      escapes = {8 => "\\b", 9 => "\\t", 10 => "\\n", 12 => "\\f", 13 => "\\r", 34 => '\\"', 92 => "\\\\"}
      value = (0..127).to_a.pack("C*").freeze
      expected = (0..127).map do |byte|
        escapes.fetch(byte) { byte < 32 ? format("\\u%04x", byte) : byte.chr }
      end.join

      @prot.write_string(value)

      expect(@trans.read(@trans.available)).to eq('"' + expected + '"')
      expect(value.encoding).to eq(Encoding::BINARY)
    end

    it "preserves character hooks and transport write boundaries" do
      transport = double("Transport")
      protocol = Thrift::JsonProtocol.new(transport)
      value = "a\u{1f600}\\\"\n\x01"

      value.each_char do |ch|
        expect(protocol).to receive(:write_json_char).with(ch).and_call_original
      end
      expect(protocol).to receive(:write_json_escape_char).with("\x01").and_call_original
      expected = ['"', "a", "\u{1f600}", "\\", "\\", "\\", '"', "\\", "n", "\\u", "0001", '"']
      expected.each do |str|
        encoding = str == "0001" ? Encoding::US_ASCII : Encoding::UTF_8
        expect(transport).to receive(:write).with(eq(str).and(have_attributes(encoding: encoding))).ordered.and_return(:written)
      end

      expect(protocol.write_string(value)).to eq(:written)
    end

    it "propagates transport exceptions at the same escape boundary" do
      failure = IOError.new("write failed")
      allow(@trans).to receive(:write).and_call_original
      allow(@trans).to receive(:write).with("0001").and_raise(failure)

      expect { @prot.write_string("a\x01z") }.to raise_error(failure.class) { |error| expect(error).to equal(failure) }
      expect(@trans.read(@trans.available)).to eq('"a\u')
    end

    it "preserves string and numeric map key contexts" do
      @prot.write_map_begin(Thrift::Types::I32, Thrift::Types::STRING, 1)
      @prot.write_i32(42)
      @prot.write_string("line\n\u{1f600}")
      @prot.write_map_end
      expect(@trans.read(@trans.available)).to eq("[\"i32\",\"str\",1,{\"42\":\"line\\n\u{1f600}\"}]".b)

      @prot.write_map_begin(Thrift::Types::STRING, Thrift::Types::DOUBLE, 1)
      @prot.write_string("key\"")
      @prot.write_double(1.5)
      @prot.write_map_end
      expect(@trans.read(@trans.available)).to eq("[\"str\",\"dbl\",1,{\"key\\\"\":1.5}]")
    end

    it "transcodes frozen UTF-16 strings without changing their ownership" do
      value = "a\n\u{1f600}".encode(Encoding::UTF_16LE).freeze
      @prot.write_string(value)
      expect(@trans.read(@trans.available)).to eq("\"a\\n\u{1f600}\"".b)
      expect(value.encoding).to eq(Encoding::UTF_16LE)
      expect(value).to eq("a\n\u{1f600}".encode(Encoding::UTF_16LE))
    end

    it "rejects invalid binary and untranscodable strings before changing map context" do
      ["\xFF".b, "\x00\xD8".b.force_encoding(Encoding::UTF_16LE)].each do |value|
        trans = Thrift::MemoryBufferTransport.new
        protocol = Thrift::JsonProtocol.new(trans)
        protocol.write_map_begin(Thrift::Types::STRING, Thrift::Types::I32, 1)
        expect { protocol.write_string(value) }.to raise_error(Thrift::ProtocolException, "Invalid UTF-8 string")
        protocol.write_string("ok")
        protocol.write_i32(1)
        protocol.write_map_end
        expect(trans.read(trans.available)).to eq('["str","i32",1,{"ok":1}]')
      end
    end

    it "should write json base64" do
      @prot.write_json_base64("this is a base64 string")
      expect(@trans.read(@trans.available)).to eq("\"dGhpcyBpcyBhIGJhc2U2NCBzdHJpbmc=\"")
    end

    it "should write json integer" do
      @prot.write_json_integer(45)
      expect(@trans.read(@trans.available)).to eq("45")

      @prot.write_json_integer(33000)
      expect(@trans.read(@trans.available)).to eq("33000")

      @prot.write_json_integer(3000000000)
      expect(@trans.read(@trans.available)).to eq("3000000000")

      @prot.write_json_integer(6000000000)
      expect(@trans.read(@trans.available)).to eq("6000000000")
    end

    it "should write json double" do
      @prot.write_json_double(12.3)
      expect(@trans.read(@trans.available)).to eq("12.3")

      @prot.write_json_double(-3.21)
      expect(@trans.read(@trans.available)).to eq("-3.21")

      @prot.write_json_double((+1.0 / 0.0) / (+1.0 / 0.0))
      expect(@trans.read(@trans.available)).to eq("\"NaN\"")

      @prot.write_json_double(+1.0 / 0.0)
      expect(@trans.read(@trans.available)).to eq("\"Infinity\"")

      @prot.write_json_double(-1.0 / 0.0)
      expect(@trans.read(@trans.available)).to eq("\"-Infinity\"")
    end

    it "should write json object start" do
      @prot.write_json_object_start
      expect(@trans.read(@trans.available)).to eq("{")
    end

    it "should write json object end" do
      @prot.write_json_object_end
      expect(@trans.read(@trans.available)).to eq("}")
    end

    it "should write json array start" do
      @prot.write_json_array_start
      expect(@trans.read(@trans.available)).to eq("[")
    end

    it "should write json array end" do
      @prot.write_json_array_end
      expect(@trans.read(@trans.available)).to eq("]")
    end

    it "should write message begin" do
      @prot.write_message_begin("name", 12, 32)
      expect(@trans.read(@trans.available)).to eq("[1,\"name\",12,32")
    end

    it "should write message end" do
      @prot.write_message_end
      expect(@trans.read(@trans.available)).to eq("]")
    end

    it "should write struct begin" do
      @prot.write_struct_begin("name")
      expect(@trans.read(@trans.available)).to eq("{")
    end

    it "should write struct end" do
      @prot.write_struct_end
      expect(@trans.read(@trans.available)).to eq("}")
    end

    it "should write field begin" do
      @prot.write_field_begin("name", Thrift::Types::STRUCT, 32)
      expect(@trans.read(@trans.available)).to eq("32{\"rec\"")
    end

    it "should write field end" do
      @prot.write_field_end
      expect(@trans.read(@trans.available)).to eq("}")
    end

    it "should write field stop" do
      @prot.write_field_stop
      expect(@trans.read(@trans.available)).to eq("")
    end

    it "should write map begin" do
      @prot.write_map_begin(Thrift::Types::STRUCT, Thrift::Types::LIST, 32)
      expect(@trans.read(@trans.available)).to eq("[\"rec\",\"lst\",32,{")
    end

    it "should write map end" do
      @prot.write_map_end
      expect(@trans.read(@trans.available)).to eq("}]")
    end

    it "should write list begin" do
      @prot.write_list_begin(Thrift::Types::STRUCT, 32)
      expect(@trans.read(@trans.available)).to eq("[\"rec\",32")
    end

    it "should write list end" do
      @prot.write_list_end
      expect(@trans.read(@trans.available)).to eq("]")
    end

    it "should write set begin" do
      @prot.write_set_begin(Thrift::Types::STRUCT, 32)
      expect(@trans.read(@trans.available)).to eq("[\"rec\",32")
    end

    it "should write set end" do
      @prot.write_set_end
      expect(@trans.read(@trans.available)).to eq("]")
    end

    it "should write bool" do
      @prot.write_bool(true)
      expect(@trans.read(@trans.available)).to eq("1")

      @prot.write_bool(false)
      expect(@trans.read(@trans.available)).to eq("0")
    end

    it "should write byte" do
      @prot.write_byte(100)
      expect(@trans.read(@trans.available)).to eq("100")
    end

    it "should write i16" do
      @prot.write_i16(1000)
      expect(@trans.read(@trans.available)).to eq("1000")
    end

    it "should write i32" do
      @prot.write_i32(2_000_000_000)
      expect(@trans.read(@trans.available)).to eq("2000000000")
    end

    it "should write i64" do
      @prot.write_i64(6000000000)
      expect(@trans.read(@trans.available)).to eq("6000000000")
    end

    it "enforces signed Thrift integer ranges before writing" do
      {
        write_byte: [-(2**7), (2**7) - 1],
        write_i16: [-(2**15), (2**15) - 1],
        write_i32: [-(2**31), (2**31) - 1],
        write_i64: [-(2**63), (2**63) - 1],
      }.each do |method, (minimum, maximum)|
        [minimum, maximum].each do |value|
          trans = Thrift::MemoryBufferTransport.new
          protocol = Thrift::JsonProtocol.new(trans)

          protocol.public_send(method, value)

          expect(trans.read(trans.available)).to eq(value.to_s)
        end

        [minimum - 1, maximum + 1].each do |value|
          trans = Thrift::MemoryBufferTransport.new
          protocol = Thrift::JsonProtocol.new(trans)

          expect { protocol.public_send(method, value) }.to raise_error(RangeError)
          expect(trans.available).to eq(0)
        end
      end
    end

    it "does not advance field or collection output after rejecting an integer" do
      @prot.write_struct_begin("Struct")
      @prot.write_field_begin("value", Thrift::Types::I16, 1)
      field_bytes = @trans.available

      expect { @prot.write_i16(2**15) }.to raise_error(RangeError)
      expect(@trans.available).to eq(field_bytes)

      trans = Thrift::MemoryBufferTransport.new
      protocol = Thrift::JsonProtocol.new(trans)
      protocol.write_list_begin(Thrift::Types::I16, 1)
      collection_bytes = trans.available

      expect { protocol.write_i16(-(2**15) - 1) }.to raise_error(RangeError)
      expect(trans.available).to eq(collection_bytes)
    end

    {nil: nil, string: "1", float: 1.0, boolean: true, object: Object.new}.each do |label, value|
      it "rejects #{label} integer arguments with Ruby TypeError before writing" do
        calls = [
          [:write_byte, []], [:write_i16, []], [:write_i32, []], [:write_i64, []],
          [:write_message_begin, ["method", Thrift::MessageTypes::CALL]],
          [:write_field_begin, ["field", Thrift::Types::I16]],
          [:write_map_begin, [Thrift::Types::I16, Thrift::Types::I16]],
          [:write_list_begin, [Thrift::Types::I16]],
          [:write_set_begin, [Thrift::Types::I16]],
        ]
        calls.each do |method, arguments|
          transport = Thrift::MemoryBufferTransport.new
          protocol = Thrift::JsonProtocol.new(transport)

          expect { protocol.public_send(method, *arguments, value) }.to raise_error(::TypeError) do |error|
            expect(error.class).to eq(::TypeError)
            expect(error.message).to eq("integer argument expected")
          end
          expect(transport.available).to eq(0)
        end
      end
    end

    it "validates integer metadata ranges before writing any output" do
      [
        [:write_message_begin, ["method", Thrift::MessageTypes::CALL], -(2**31), (2**31) - 1],
        [:write_field_begin, ["field", Thrift::Types::I16], -(2**15), (2**15) - 1],
        [:write_map_begin, [Thrift::Types::I16, Thrift::Types::I16], 0, (2**31) - 1],
        [:write_list_begin, [Thrift::Types::I16], 0, (2**31) - 1],
        [:write_set_begin, [Thrift::Types::I16], 0, (2**31) - 1],
      ].each do |method, arguments, minimum, maximum|
        [minimum - 1, maximum + 1].each do |value|
          transport = Thrift::MemoryBufferTransport.new
          protocol = Thrift::JsonProtocol.new(transport)

          expect { protocol.public_send(method, *arguments, value) }.to raise_error(RangeError)
          expect(transport.available).to eq(0)
        end
      end
    end

    it "should write double" do
      @prot.write_double(1.23)
      expect(@trans.read(@trans.available)).to eq("1.23")

      @prot.write_double(-32.1)
      expect(@trans.read(@trans.available)).to eq("-32.1")

      @prot.write_double((+1.0 / 0.0) / (+1.0 / 0.0))
      expect(@trans.read(@trans.available)).to eq("\"NaN\"")

      @prot.write_double(+1.0 / 0.0)
      expect(@trans.read(@trans.available)).to eq("\"Infinity\"")

      @prot.write_double(-1.0 / 0.0)
      expect(@trans.read(@trans.available)).to eq("\"-Infinity\"")
    end

    it "should write string" do
      @prot.write_string("this is a test string")
      a = @trans.read(@trans.available)
      expect(a).to eq('"this is a test string"'.b)
      expect(a.encoding).to eq(Encoding::BINARY)
    end

    it "should write string with unicode characters" do
      @prot.write_string("this is a test string with unicode characters: \u20AC \u20AD")
      a = @trans.read(@trans.available)
      expect(a).to eq("\"this is a test string with unicode characters: \u20AC \u20AD\"".b)
      expect(a.encoding).to eq(Encoding::BINARY)
    end

    it "transcodes non-UTF-8 strings before writing JSON" do
      value = +"caf\xE9"
      value.force_encoding(Encoding::ISO_8859_1)

      @prot.write_string(value)
      data = @trans.read(@trans.available)

      expect(data).to eq("\"caf\u00E9\"".encode(Encoding::UTF_8).b)
      expect(data.dup.force_encoding(Encoding::UTF_8)).to be_valid_encoding
      expect(value.encoding).to eq(Encoding::ISO_8859_1)
    end

    it "writes valid UTF-8 stored in a binary string" do
      value = "caf\xC3\xA9".b

      @prot.write_string(value)
      data = @trans.read(@trans.available)

      expect(data).to eq("\"caf\xC3\xA9\"".b)
      expect(value.encoding).to eq(Encoding::BINARY)
    end

    it "rejects invalid UTF-8 before writing any JSON bytes" do
      value = "\xC3\x28".b.force_encoding(Encoding::UTF_8)

      expect { @prot.write_string(value) }.to raise_error(Thrift::ProtocolException) do |error|
        expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
        expect(error.message).to eq("Invalid UTF-8 string")
      end
      expect(@trans.available).to eq(0)
    end

    it "should write binary" do
      @prot.write_binary("this is a base64 string")
      expect(@trans.read(@trans.available)).to eq("\"dGhpcyBpcyBhIGJhc2U2NCBzdHJpbmc=\"")
    end

    it "should write long binary" do
      @prot.write_binary((0...256).to_a.pack("C*"))
      expect(@trans.read(@trans.available)).to eq("\"AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/w==\"")
    end

    it "should write a uuid" do
      @prot.write_uuid("00112233-4455-6677-8899-aabbccddeeff")
      expect(@trans.read(@trans.available)).to eq("\"00112233-4455-6677-8899-aabbccddeeff\"")
    end

    it "should get type name for type id" do
      expect { @prot.get_type_name_for_type_id(Thrift::Types::STOP) }.to raise_error(Thrift::ProtocolException, "Unknown type id: 0")
      expect { @prot.get_type_name_for_type_id(Thrift::Types::VOID) }.to raise_error(Thrift::ProtocolException, "Unknown type id: 1")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::BOOL)).to eq("tf")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::BYTE)).to eq("i8")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::DOUBLE)).to eq("dbl")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::I16)).to eq("i16")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::I32)).to eq("i32")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::I64)).to eq("i64")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::STRING)).to eq("str")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::STRUCT)).to eq("rec")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::MAP)).to eq("map")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::SET)).to eq("set")
      expect(@prot.get_type_name_for_type_id(Thrift::Types::LIST)).to eq("lst")
    end

    it "should get type id for type name" do
      expect { @prot.get_type_id_for_type_name("pp") }.to raise_error(Thrift::ProtocolException, 'Unknown type name: "pp"')
      expect(@prot.get_type_id_for_type_name("tf")).to eq(Thrift::Types::BOOL)
      expect(@prot.get_type_id_for_type_name("i8")).to eq(Thrift::Types::BYTE)
      expect(@prot.get_type_id_for_type_name("dbl")).to eq(Thrift::Types::DOUBLE)
      expect(@prot.get_type_id_for_type_name("i16")).to eq(Thrift::Types::I16)
      expect(@prot.get_type_id_for_type_name("i32")).to eq(Thrift::Types::I32)
      expect(@prot.get_type_id_for_type_name("i64")).to eq(Thrift::Types::I64)
      expect(@prot.get_type_id_for_type_name("str")).to eq(Thrift::Types::STRING)
      expect(@prot.get_type_id_for_type_name("rec")).to eq(Thrift::Types::STRUCT)
      expect(@prot.get_type_id_for_type_name("map")).to eq(Thrift::Types::MAP)
      expect(@prot.get_type_id_for_type_name("set")).to eq(Thrift::Types::SET)
      expect(@prot.get_type_id_for_type_name("lst")).to eq(Thrift::Types::LIST)
    end

    it "should reject unknown field and container types as invalid protocol data" do
      @trans.write('{"1":{"wat":0}}')
      @prot.read_struct_begin
      expect { @prot.read_field_begin }.to raise_error(Thrift::ProtocolException, 'Unknown type name: "wat"') do |error|
        expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
      end

      trans = Thrift::MemoryBufferTransport.new('["wat",0]')
      prot = Thrift::JsonProtocol.new(trans)
      expect { prot.read_list_begin }.to raise_error(Thrift::ProtocolException, 'Unknown type name: "wat"') do |error|
        expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
      end
    end

    it "should read json syntax char" do
      @trans.write("F")
      expect { @prot.read_json_syntax_char("G") }.to raise_error(Thrift::ProtocolException)
      @trans.write("H")
      @prot.read_json_syntax_char("H")
    end

    it "should read json escape char" do
      @trans.write("0054")
      expect(@prot.read_json_escape_char).to eq("T")

      @trans.write("\"\\\"\"")
      expect(@prot.read_json_string(false)).to eq("\"")

      @trans.write("\"\\\\\"")
      expect(@prot.read_json_string(false)).to eq("\\")

      @trans.write("\"\\/\"")
      expect(@prot.read_json_string(false)).to eq("/")

      @trans.write("\"\\b\"")
      expect(@prot.read_json_string(false)).to eq("\b")

      @trans.write("\"\\f\"")
      expect(@prot.read_json_string(false)).to eq("\f")

      @trans.write("\"\\n\"")
      expect(@prot.read_json_string(false)).to eq("\n")

      @trans.write("\"\\r\"")
      expect(@prot.read_json_string(false)).to eq("\r")

      @trans.write("\"\\t\"")
      expect(@prot.read_json_string(false)).to eq("\t")
    end

    it "decodes BMP and supplementary Unicode escapes" do
      {
        '"\u20AC"' => "\u20AC",
        '"\uD83D\uDE00"' => "\u{1F600}",
        '"\ud83d\uDe00"' => "\u{1F600}",
      }.each do |wire, value|
        trans = Thrift::MemoryBufferTransport.new(wire.b)
        protocol = Thrift::JsonProtocol.new(trans)

        expect(protocol.read_json_string).to eq(value)
      end
    end

    it "decodes mixed raw UTF-8 and escaped Unicode" do
      @trans.write("\"raw \u{1F600} and \\u20AC\"".encode(Encoding::UTF_8).b)

      value = @prot.read_json_string

      expect(value).to eq("raw \u{1F600} and \u20AC")
      expect(value.encoding).to eq(Encoding::UTF_8)
      expect(value).to be_valid_encoding
    end

    it "rejects malformed Unicode escapes and unpaired surrogates" do
      [
        '"\u12G4"',
        '"\u123"',
        '"\uD83D"',
        '"\uD83D\u0041"',
        '"\uDE00"',
      ].each do |wire|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new(wire.b))

        expect { protocol.read_json_string }.to raise_error(Thrift::ProtocolException) do |error|
          expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
        end
      end
    end

    it "rejects invalid raw UTF-8 in JSON strings" do
      @trans.write("\"\xC3\x28\"".b)

      expect { @prot.read_json_string }.to raise_error(Thrift::ProtocolException) do |error|
        expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
        expect(error.message).to eq("Invalid UTF-8 string")
      end
    end

    it "should read json string" do
      @trans.write("\"\\P")
      expect { @prot.read_json_string(false) }.to raise_error(Thrift::ProtocolException)

      @trans.write("\"this is a test string\"")
      expect(@prot.read_json_string).to eq("this is a test string")
    end

    it "should read json base64" do
      @trans.write("\"dGhpcyBpcyBhIHRlc3Qgc3RyaW5n\"")
      expect(@prot.read_json_base64).to eq("this is a test string")
    end

    it "rejects invalid Base64 alphabet and padding" do
      ['"%"', '"Y=Q="'].each do |wire|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new(wire))

        expect { protocol.read_json_base64 }.to raise_error(Thrift::ProtocolException, "Invalid Base64 data") do |error|
          expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
        end
      end
    end

    it "should is json numeric" do
      expect(@prot.is_json_numeric("A")).to eq(false)
      expect(@prot.is_json_numeric("+")).to eq(true)
      expect(@prot.is_json_numeric("-")).to eq(true)
      expect(@prot.is_json_numeric(".")).to eq(true)
      expect(@prot.is_json_numeric("0")).to eq(true)
      expect(@prot.is_json_numeric("1")).to eq(true)
      expect(@prot.is_json_numeric("2")).to eq(true)
      expect(@prot.is_json_numeric("3")).to eq(true)
      expect(@prot.is_json_numeric("4")).to eq(true)
      expect(@prot.is_json_numeric("5")).to eq(true)
      expect(@prot.is_json_numeric("6")).to eq(true)
      expect(@prot.is_json_numeric("7")).to eq(true)
      expect(@prot.is_json_numeric("8")).to eq(true)
      expect(@prot.is_json_numeric("9")).to eq(true)
      expect(@prot.is_json_numeric("E")).to eq(true)
      expect(@prot.is_json_numeric("e")).to eq(true)
    end

    it "should read json numeric chars" do
      @trans.write("1.453E45T")
      expect(@prot.read_json_numeric_chars).to eq("1.453E45")
    end

    it "should read json integer" do
      @trans.write("1.45\"\"")
      expect { @prot.read_json_integer }.to raise_error(Thrift::ProtocolException)
      @prot.read_string

      @trans.write("1453T")
      expect(@prot.read_json_integer).to eq(1453)
    end

    it "rejects non-canonical JSON integer spellings" do
      %w[+1 01 00 -01 -00 - --1 1.0 1e2 .5 1+2].each do |wire|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("#{wire}]"))

        expect { protocol.read_json_integer }.to raise_error(Thrift::ProtocolException) do |error|
          expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
        end
      end
    end

    it "accepts the complete JSON number grammar for doubles" do
      {
        "0" => 0.0,
        "-0" => -0.0,
        "0.0" => 0.0,
        "1.25" => 1.25,
        "1e2" => 100.0,
        "1e+2" => 100.0,
        "-1.25E-3" => -0.00125,
      }.each do |wire, value|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("#{wire}]"))

        expect(protocol.read_json_double).to eq(value)
      end
    end

    it "rejects non-JSON double spellings" do
      %w[+1 01 00 -01 -00 .5 1. 1e 1e+ --1 1..2 1e2e3].each do |wire|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("#{wire}]"))

        expect { protocol.read_json_double }.to raise_error(Thrift::ProtocolException) do |error|
          expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
        end
      end
    end

    it "preserves quoted JSON numbers for map keys" do
      @trans.write('["i16","i16",1,{"1":2}]')

      expect(@prot.read_map_begin).to eq([Thrift::Types::I16, Thrift::Types::I16, 1])
      expect(@prot.read_i16).to eq(1)
      expect(@prot.read_i16).to eq(2)
      expect(@prot.read_map_end).to be_nil
    end

    it "validates the grammar of quoted integer and double map keys" do
      {
        "i16" => [:read_i16, %w[+1 01 1.0 1e2]],
        "dbl" => [:read_double, %w[+1 01 .5 1. 1e+]],
      }.each do |type, (method, invalid_keys)|
        invalid_keys.each do |key|
          protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("[\"#{type}\",\"i16\",1,{\"#{key}\":2}]"))
          protocol.read_map_begin

          expect { protocol.public_send(method) }.to raise_error(Thrift::ProtocolException) do |error|
            expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
          end
        end
      end

      @trans.write('["dbl","i16",1,{"-1.25E+2":2}]')
      @prot.read_map_begin
      expect(@prot.read_double).to eq(-125.0)
      expect(@prot.read_i16).to eq(2)
      expect(@prot.read_map_end).to be_nil
    end

    it "can resume at the next list value after rejecting a malformed number" do
      @trans.write('["i16",2,+1,2]')
      expect(@prot.read_list_begin).to eq([Thrift::Types::I16, 2])

      expect { @prot.read_i16 }.to raise_error(Thrift::ProtocolException)
      expect(@prot.read_i16).to eq(2)
      expect(@prot.read_list_end).to be_nil
    end

    it "rejects a malformed number terminator when the enclosing value ends" do
      @trans.write('["i16",1,1x]')
      expect(@prot.read_list_begin).to eq([Thrift::Types::I16, 1])
      expect(@prot.read_i16).to eq(1)

      expect { @prot.read_list_end }.to raise_error(Thrift::ProtocolException)
    end

    it "should read json double" do
      @trans.write("1.45e3e01\"\"")
      expect { @prot.read_json_double }.to raise_error(Thrift::ProtocolException)
      @prot.read_string

      @trans.write("\"1.453e01\"")
      expect { @prot.read_json_double }.to raise_error(Thrift::ProtocolException)

      @trans.write("1.453e01\"\"")
      expect(@prot.read_json_double).to eq(14.53)
      @prot.read_string

      @trans.write("\"NaN\"")
      expect(@prot.read_json_double.nan?).to eq(true)

      @trans.write("\"Infinity\"")
      expect(@prot.read_json_double).to eq(+1.0 / 0.0)

      @trans.write("\"-Infinity\"")
      expect(@prot.read_json_double).to eq(-1.0 / 0.0)
    end

    it "should read json object start" do
      @trans.write("{")
      expect(@prot.read_json_object_start).to eq(nil)
    end

    it "should read json object end" do
      @trans.write("}")
      expect(@prot.read_json_object_end).to eq(nil)
    end

    it "should read json array start" do
      @trans.write("[")
      expect(@prot.read_json_array_start).to eq(nil)
    end

    it "should read json array end" do
      @trans.write("]")
      expect(@prot.read_json_array_end).to eq(nil)
    end

    it "should read_message_begin" do
      @trans.write("[2,")
      expect { @prot.read_message_begin }.to raise_error(Thrift::ProtocolException)

      @trans.write("[1,\"name\",12,32\"\"")
      expect(@prot.read_message_begin).to eq(["name", 12, 32])
    end

    it "should read message end" do
      @trans.write("]")
      expect(@prot.read_message_end).to eq(nil)
    end

    it "should read struct begin" do
      @trans.write("{")
      expect(@prot.read_struct_begin).to eq(nil)
    end

    it "should read struct end" do
      @trans.write("}")
      expect(@prot.read_struct_end).to eq(nil)
    end

    it "should read field begin" do
      @trans.write("1{\"rec\"")
      expect(@prot.read_field_begin).to eq([nil, 12, 1])
    end

    it "should read field end" do
      @trans.write("}")
      expect(@prot.read_field_end).to eq(nil)
    end

    it "should read map begin" do
      @trans.write("[\"rec\",\"lst\",2,{")
      expect(@prot.read_map_begin).to eq([12, 15, 2])
    end

    it "should reject a negative map size" do
      @trans.write("[\"rec\",\"lst\",-1,{")
      expect { @prot.read_map_begin }.to raise_error(Thrift::ProtocolException, "Negative size") do |e|
        expect(e.type).to eq(Thrift::ProtocolException::NEGATIVE_SIZE)
      end
    end

    it "should read map end" do
      @trans.write("}]")
      expect(@prot.read_map_end).to eq(nil)
    end

    it "should read list begin" do
      @trans.write("[\"rec\",2\"\"")
      expect(@prot.read_list_begin).to eq([12, 2])
    end

    it "should reject a negative list size" do
      @trans.write("[\"rec\",-1\"\"")
      expect { @prot.read_list_begin }.to raise_error(Thrift::ProtocolException, "Negative size") do |e|
        expect(e.type).to eq(Thrift::ProtocolException::NEGATIVE_SIZE)
      end
    end

    it "should read list end" do
      @trans.write("]")
      expect(@prot.read_list_end).to eq(nil)
    end

    it "should read set begin" do
      @trans.write("[\"rec\",2\"\"")
      expect(@prot.read_set_begin).to eq([12, 2])
    end

    it "should reject a negative set size" do
      @trans.write("[\"rec\",-1\"\"")
      expect { @prot.read_set_begin }.to raise_error(Thrift::ProtocolException, "Negative size") do |e|
        expect(e.type).to eq(Thrift::ProtocolException::NEGATIVE_SIZE)
      end
    end

    it "should read set end" do
      @trans.write("]")
      expect(@prot.read_set_end).to eq(nil)
    end

    it "should skip unknown string fields without base64 decoding" do
      prot = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new('{"2":{"str":"%"}}'))
      hello = SpecNamespace::Hello.new

      expect {
        hello.read(prot)
      }.not_to raise_error
      expect(hello.greeting).to eq("hello world")
    end

    it "should skip unknown string fields through protocol decorators" do
      trans = Thrift::MemoryBufferTransport.new('{"2":{"str":"%"}}')
      prot = Thrift::MultiplexedProtocol.new(Thrift::JsonProtocol.new(trans), "service")
      hello = SpecNamespace::Hello.new

      expect {
        hello.read(prot)
      }.not_to raise_error
      expect(hello.greeting).to eq("hello world")
    end

    it "should read bool" do
      [0, 1, -1, 128, -129].each do |value|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("#{value}]"))

        expect(protocol.read_bool).to eq(value != 0)
      end
    end

    it "should read byte" do
      @trans.write("60\"\"")
      expect(@prot.read_byte).to eq(60)
    end

    it "should read i16" do
      @trans.write("1000\"\"")
      expect(@prot.read_i16).to eq(1000)
    end

    it "should read i32" do
      @trans.write("2000000000\"\"")
      expect(@prot.read_i32).to eq(2000000000)
    end

    it "should read i64" do
      @trans.write("6000000000\"\"")
      expect(@prot.read_i64).to eq(6000000000)
    end

    it "enforces signed Thrift integer ranges while reading" do
      {
        read_byte: [-(2**7), (2**7) - 1],
        read_i16: [-(2**15), (2**15) - 1],
        read_i32: [-(2**31), (2**31) - 1],
        read_i64: [-(2**63), (2**63) - 1],
      }.each do |method, (minimum, maximum)|
        [minimum, maximum].each do |value|
          protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("#{value}]"))

          expect(protocol.public_send(method)).to eq(value)
        end

        [minimum - 1, maximum + 1].each do |value|
          protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("#{value}]"))

          expect { protocol.public_send(method) }.to raise_error(Thrift::ProtocolException) do |error|
            expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
          end
        end
      end
    end

    it "preserves the typed errors for invalid collection sizes" do
      {
        read_map_begin: '["i16","i16",%d,{}]',
        read_list_begin: '["i16",%d]',
        read_set_begin: '["i16",%d]',
      }.each do |method, wire|
        {
          -(2**31) - 1 => Thrift::ProtocolException::NEGATIVE_SIZE,
          -1 => Thrift::ProtocolException::NEGATIVE_SIZE,
          2**31 => Thrift::ProtocolException::SIZE_LIMIT,
        }.each do |size, type|
          protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new(wire % size))

          expect { protocol.public_send(method) }.to raise_error(Thrift::ProtocolException) do |error|
            expect(error.type).to eq(type)
          end
        end
      end
    end

    it "validates field and sequence IDs at their declared widths" do
      [-32768, 32767].each do |id|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("{\"#{id}\":{\"i16\":0}}"))
        protocol.read_struct_begin
        expect(protocol.read_field_begin).to eq([nil, Thrift::Types::I16, id])
      end
      [-32769, 32768].each do |id|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("{\"#{id}\":{\"i16\":0}}"))
        protocol.read_struct_begin
        expect { protocol.read_field_begin }.to raise_error(Thrift::ProtocolException)
      end
      [-(2**31), (2**31) - 1].each do |seqid|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("[1,\"method\",1,#{seqid},{}]"))
        expect(protocol.read_message_begin).to eq(["method", Thrift::MessageTypes::CALL, seqid])
      end
      [-(2**31) - 1, 2**31].each do |seqid|
        protocol = Thrift::JsonProtocol.new(Thrift::MemoryBufferTransport.new("[1,\"method\",1,#{seqid},{}]"))
        expect { protocol.read_message_begin }.to raise_error(Thrift::ProtocolException)
      end
    end

    it "should read double" do
      @trans.write("12.23\"\"")
      expect(@prot.read_double).to eq(12.23)
    end

    it "should read string" do
      @trans.write('"this is a test string"'.b)
      a = @prot.read_string
      expect(a).to eq("this is a test string")
      expect(a.encoding).to eq(Encoding::UTF_8)
    end

    it "should read string with unicode characters" do
      @trans.write('"this is a test string with unicode characters: \u20AC \u20AD"'.b)
      a = @prot.read_string
      expect(a).to eq("this is a test string with unicode characters: \u20AC \u20AD")
      expect(a.encoding).to eq(Encoding::UTF_8)
    end

    it "should read binary" do
      @trans.write("\"dGhpcyBpcyBhIHRlc3Qgc3RyaW5n\"")
      expect(@prot.read_binary).to eq("this is a test string")
    end

    it "should read long binary" do
      @trans.write("\"AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/w==\"")
      expect(@prot.read_binary.bytes.to_a).to eq((0...256).to_a)
    end

    it "should read a uuid" do
      @trans.write("\"00112233-4455-6677-8899-aabbccddeeff\"")
      expect(@prot.read_uuid).to eq("00112233-4455-6677-8899-aabbccddeeff")
    end

    it "should normalize uppercase uuid on read" do
      @trans.write("\"00112233-4455-6677-8899-AABBCCDDEEFF\"")
      expect(@prot.read_uuid).to eq("00112233-4455-6677-8899-aabbccddeeff")
    end

    it "should provide a reasonable to_s" do
      expect(@prot.to_s).to eq("json(memory)")
    end
  end

  describe Thrift::JsonProtocolFactory do
    it "should create a JsonProtocol" do
      expect(Thrift::JsonProtocolFactory.new.get_protocol(double("MockTransport"))).to be_instance_of(Thrift::JsonProtocol)
    end

    it "should provide a reasonable to_s" do
      expect(Thrift::JsonProtocolFactory.new.to_s).to eq("json")
    end
  end
end
