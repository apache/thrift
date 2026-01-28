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
require_relative 'support/header_protocol_helper'

describe 'HeaderProtocol' do
  include HeaderProtocolHelper

  describe Thrift::HeaderProtocol do
    before(:each) do
      @buffer = Thrift::MemoryBufferTransport.new
      @protocol = Thrift::HeaderProtocol.new(@buffer)
    end

    it "should provide a to_s" do
      expect(@protocol.to_s).to match(/header\(compact/)
    end

    it "should wrap transport in HeaderTransport" do
      expect(@protocol.trans).to be_a(Thrift::HeaderTransport)
    end

    it "should use existing HeaderTransport if passed" do
      header_trans = Thrift::HeaderTransport.new(@buffer)
      protocol = Thrift::HeaderProtocol.new(header_trans)
      expect(protocol.trans).to equal(header_trans)
    end

    describe "header management delegation" do
      it "should delegate get_headers" do
        # Write with headers and read back to populate read headers
        @protocol.set_header("key", "value")
        @protocol.write_message_begin("test", Thrift::MessageTypes::CALL, 1)
        @protocol.write_struct_begin("args")
        @protocol.write_field_stop
        @protocol.write_struct_end
        @protocol.write_message_end
        @protocol.trans.flush

        # Read back
        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer)

        read_protocol.read_message_begin
        headers = read_protocol.get_headers
        expect(headers["key"]).to eq("value")
      end

      it "should delegate set_header" do
        expect(@protocol.trans).to receive(:set_header).with("key", "value")
        @protocol.set_header("key", "value")
      end

      it "should delegate clear_headers" do
        expect(@protocol.trans).to receive(:clear_headers)
        @protocol.clear_headers
      end

      it "should delegate add_transform" do
        expect(@protocol.trans).to receive(:add_transform).with(Thrift::HeaderTransformID::ZLIB)
        @protocol.add_transform(Thrift::HeaderTransformID::ZLIB)
      end
    end

    describe "protocol delegation with Binary protocol" do
      before(:each) do
        @buffer = Thrift::MemoryBufferTransport.new
        @protocol = Thrift::HeaderProtocol.new(@buffer, nil, Thrift::HeaderSubprotocolID::BINARY)
      end

      it "should write message begin" do
        @protocol.write_message_begin("test_method", Thrift::MessageTypes::CALL, 123)
        @protocol.write_message_end
        @protocol.trans.flush

        # Verify we can read it back
        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer)

        name, type, seqid = read_protocol.read_message_begin
        expect(name).to eq("test_method")
        expect(type).to eq(Thrift::MessageTypes::CALL)
        expect(seqid).to eq(123)
      end

      it "should write and read structs" do
        @protocol.write_message_begin("test", Thrift::MessageTypes::CALL, 1)
        @protocol.write_struct_begin("TestStruct")
        @protocol.write_field_begin("field1", Thrift::Types::I32, 1)
        @protocol.write_i32(42)
        @protocol.write_field_end
        @protocol.write_field_stop
        @protocol.write_struct_end
        @protocol.write_message_end
        @protocol.trans.flush

        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer)

        read_protocol.read_message_begin
        read_protocol.read_struct_begin
        name, type, id = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::I32)
        expect(id).to eq(1)
        value = read_protocol.read_i32
        expect(value).to eq(42)
      end

      it "should write and read all primitive types" do
        @protocol.write_message_begin("test", Thrift::MessageTypes::CALL, 1)
        @protocol.write_struct_begin("TestStruct")

        @protocol.write_field_begin("bool", Thrift::Types::BOOL, 1)
        @protocol.write_bool(true)
        @protocol.write_field_end

        @protocol.write_field_begin("byte", Thrift::Types::BYTE, 2)
        @protocol.write_byte(127)
        @protocol.write_field_end

        @protocol.write_field_begin("i16", Thrift::Types::I16, 3)
        @protocol.write_i16(32767)
        @protocol.write_field_end

        @protocol.write_field_begin("i32", Thrift::Types::I32, 4)
        @protocol.write_i32(2147483647)
        @protocol.write_field_end

        @protocol.write_field_begin("i64", Thrift::Types::I64, 5)
        @protocol.write_i64(9223372036854775807)
        @protocol.write_field_end

        @protocol.write_field_begin("double", Thrift::Types::DOUBLE, 6)
        @protocol.write_double(3.14159)
        @protocol.write_field_end

        @protocol.write_field_begin("string", Thrift::Types::STRING, 7)
        @protocol.write_string("hello")
        @protocol.write_field_end

        @protocol.write_field_stop
        @protocol.write_struct_end
        @protocol.write_message_end
        @protocol.trans.flush

        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer)

        read_protocol.read_message_begin
        read_protocol.read_struct_begin

        # bool
        _, type, _ = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::BOOL)
        expect(read_protocol.read_bool).to eq(true)
        read_protocol.read_field_end

        # byte
        _, type, _ = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::BYTE)
        expect(read_protocol.read_byte).to eq(127)
        read_protocol.read_field_end

        # i16
        _, type, _ = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::I16)
        expect(read_protocol.read_i16).to eq(32767)
        read_protocol.read_field_end

        # i32
        _, type, _ = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::I32)
        expect(read_protocol.read_i32).to eq(2147483647)
        read_protocol.read_field_end

        # i64
        _, type, _ = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::I64)
        expect(read_protocol.read_i64).to eq(9223372036854775807)
        read_protocol.read_field_end

        # double
        _, type, _ = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::DOUBLE)
        expect(read_protocol.read_double).to be_within(0.00001).of(3.14159)
        read_protocol.read_field_end

        # string
        _, type, _ = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::STRING)
        expect(read_protocol.read_string).to eq("hello")
        read_protocol.read_field_end
      end
    end

    describe "protocol delegation with Compact protocol" do
      before(:each) do
        @buffer = Thrift::MemoryBufferTransport.new
        @protocol = Thrift::HeaderProtocol.new(
          @buffer,
          nil,
          Thrift::HeaderSubprotocolID::COMPACT
        )
      end

      it "should use Compact protocol" do
        expect(@protocol.to_s).to match(/header\(compact/)
      end

      it "should write and read with Compact protocol" do
        @protocol.write_message_begin("test", Thrift::MessageTypes::CALL, 1)
        @protocol.write_struct_begin("Test")
        @protocol.write_field_begin("field", Thrift::Types::I32, 1)
        @protocol.write_i32(999)
        @protocol.write_field_end
        @protocol.write_field_stop
        @protocol.write_struct_end
        @protocol.write_message_end
        @protocol.trans.flush

        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer, nil, Thrift::HeaderSubprotocolID::COMPACT)

        read_protocol.read_message_begin
        read_protocol.read_struct_begin
        _, type, _ = read_protocol.read_field_begin
        expect(type).to eq(Thrift::Types::I32)
        expect(read_protocol.read_i32).to eq(999)
      end
    end

    describe "unknown protocol handling" do
      it "should write an exception response on unknown protocol id" do
        header_data = +""
        header_data << varint32(0x10)
        header_data << varint32(0)
        frame = build_header_frame(header_data)

        buffer = Thrift::MemoryBufferTransport.new(frame)
        protocol = Thrift::HeaderProtocol.new(buffer)

        expect { protocol.read_message_begin }.to raise_error(Thrift::ProtocolException)

        response = buffer.read(buffer.available)
        expect(response.bytesize).to be > 0
        magic = response[4, 2].unpack('n').first
        expect(magic).to eq(Thrift::HeaderTransport::HEADER_MAGIC)
      end
    end

    describe "protocol auto-detection with legacy frames" do
      it "should detect framed compact messages" do
        write_buffer = Thrift::MemoryBufferTransport.new
        write_protocol = Thrift::CompactProtocol.new(write_buffer)

        write_protocol.write_message_begin("legacy_framed", Thrift::MessageTypes::CALL, 7)
        write_protocol.write_struct_begin("Args")
        write_protocol.write_field_stop
        write_protocol.write_struct_end
        write_protocol.write_message_end

        payload = write_buffer.read(write_buffer.available)
        framed = [payload.bytesize].pack('N') + payload

        read_buffer = Thrift::MemoryBufferTransport.new(framed)
        protocol = Thrift::HeaderProtocol.new(read_buffer)

        name, type, seqid = protocol.read_message_begin
        expect(name).to eq("legacy_framed")
        expect(type).to eq(Thrift::MessageTypes::CALL)
        expect(seqid).to eq(7)

        protocol.read_struct_begin
        _, field_type, _ = protocol.read_field_begin
        expect(field_type).to eq(Thrift::Types::STOP)
        protocol.read_struct_end
        protocol.read_message_end
      end

      it "should detect unframed compact messages" do
        write_buffer = Thrift::MemoryBufferTransport.new
        write_protocol = Thrift::CompactProtocol.new(write_buffer)

        write_protocol.write_message_begin("legacy_unframed", Thrift::MessageTypes::CALL, 9)
        write_protocol.write_struct_begin("Args")
        write_protocol.write_field_stop
        write_protocol.write_struct_end
        write_protocol.write_message_end

        payload = write_buffer.read(write_buffer.available)

        read_buffer = Thrift::MemoryBufferTransport.new(payload)
        protocol = Thrift::HeaderProtocol.new(read_buffer)

        name, type, seqid = protocol.read_message_begin
        expect(name).to eq("legacy_unframed")
        expect(type).to eq(Thrift::MessageTypes::CALL)
        expect(seqid).to eq(9)

        protocol.read_struct_begin
        _, field_type, _ = protocol.read_field_begin
        expect(field_type).to eq(Thrift::Types::STOP)
        protocol.read_struct_end
        protocol.read_message_end
      end
    end

    describe "with compression" do
      it "should work with ZLIB transform" do
        @protocol.add_transform(Thrift::HeaderTransformID::ZLIB)

        @protocol.write_message_begin("compressed_test", Thrift::MessageTypes::CALL, 42)
        @protocol.write_struct_begin("Args")
        @protocol.write_field_begin("data", Thrift::Types::STRING, 1)
        @protocol.write_string("a" * 100) # Compressible data
        @protocol.write_field_end
        @protocol.write_field_stop
        @protocol.write_struct_end
        @protocol.write_message_end
        @protocol.trans.flush

        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer)

        name, type, seqid = read_protocol.read_message_begin
        expect(name).to eq("compressed_test")
        expect(seqid).to eq(42)

        read_protocol.read_struct_begin
        _, _, _ = read_protocol.read_field_begin
        result = read_protocol.read_string
        expect(result).to eq("a" * 100)
      end
    end

    describe "containers" do
      it "should write and read lists" do
        @protocol.write_message_begin("test", Thrift::MessageTypes::CALL, 1)
        @protocol.write_struct_begin("Test")
        @protocol.write_field_begin("list", Thrift::Types::LIST, 1)
        @protocol.write_list_begin(Thrift::Types::I32, 3)
        @protocol.write_i32(1)
        @protocol.write_i32(2)
        @protocol.write_i32(3)
        @protocol.write_list_end
        @protocol.write_field_end
        @protocol.write_field_stop
        @protocol.write_struct_end
        @protocol.write_message_end
        @protocol.trans.flush

        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer)

        read_protocol.read_message_begin
        read_protocol.read_struct_begin
        _, _, _ = read_protocol.read_field_begin
        etype, size = read_protocol.read_list_begin
        expect(etype).to eq(Thrift::Types::I32)
        expect(size).to eq(3)
        expect(read_protocol.read_i32).to eq(1)
        expect(read_protocol.read_i32).to eq(2)
        expect(read_protocol.read_i32).to eq(3)
      end

      it "should write and read maps" do
        @protocol.write_message_begin("test", Thrift::MessageTypes::CALL, 1)
        @protocol.write_struct_begin("Test")
        @protocol.write_field_begin("map", Thrift::Types::MAP, 1)
        @protocol.write_map_begin(Thrift::Types::STRING, Thrift::Types::I32, 2)
        @protocol.write_string("a")
        @protocol.write_i32(1)
        @protocol.write_string("b")
        @protocol.write_i32(2)
        @protocol.write_map_end
        @protocol.write_field_end
        @protocol.write_field_stop
        @protocol.write_struct_end
        @protocol.write_message_end
        @protocol.trans.flush

        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer)

        read_protocol.read_message_begin
        read_protocol.read_struct_begin
        _, _, _ = read_protocol.read_field_begin
        ktype, vtype, size = read_protocol.read_map_begin
        expect(ktype).to eq(Thrift::Types::STRING)
        expect(vtype).to eq(Thrift::Types::I32)
        expect(size).to eq(2)
      end

      it "should write and read sets" do
        @protocol.write_message_begin("test", Thrift::MessageTypes::CALL, 1)
        @protocol.write_struct_begin("Test")
        @protocol.write_field_begin("set", Thrift::Types::SET, 1)
        @protocol.write_set_begin(Thrift::Types::STRING, 2)
        @protocol.write_string("x")
        @protocol.write_string("y")
        @protocol.write_set_end
        @protocol.write_field_end
        @protocol.write_field_stop
        @protocol.write_struct_end
        @protocol.write_message_end
        @protocol.trans.flush

        data = @buffer.read(@buffer.available)
        read_buffer = Thrift::MemoryBufferTransport.new(data)
        read_protocol = Thrift::HeaderProtocol.new(read_buffer)

        read_protocol.read_message_begin
        read_protocol.read_struct_begin
        _, _, _ = read_protocol.read_field_begin
        etype, size = read_protocol.read_set_begin
        expect(etype).to eq(Thrift::Types::STRING)
        expect(size).to eq(2)
      end
    end
  end

  describe Thrift::HeaderProtocolFactory do
    it "should create HeaderProtocol" do
      factory = Thrift::HeaderProtocolFactory.new
      buffer = Thrift::MemoryBufferTransport.new
      protocol = factory.get_protocol(buffer)
      expect(protocol).to be_a(Thrift::HeaderProtocol)
    end

    it "should provide a reasonable to_s" do
      expect(Thrift::HeaderProtocolFactory.new.to_s).to eq("header")
    end

    it "should pass configuration to protocol" do
      factory = Thrift::HeaderProtocolFactory.new(nil, Thrift::HeaderSubprotocolID::COMPACT)
      buffer = Thrift::MemoryBufferTransport.new
      protocol = factory.get_protocol(buffer)
      expect(protocol.to_s).to match(/compact/)
    end
  end
end
