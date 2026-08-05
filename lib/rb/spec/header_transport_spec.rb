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
require_relative 'support/header_protocol_helper'

describe 'HeaderTransport' do
  include HeaderProtocolHelper

  describe Thrift::HeaderClientType do
    it "should define client type constants" do
      expect(Thrift::HeaderClientType::HEADERS).to eq(0x00)
      expect(Thrift::HeaderClientType::FRAMED_BINARY).to eq(0x01)
      expect(Thrift::HeaderClientType::UNFRAMED_BINARY).to eq(0x02)
      expect(Thrift::HeaderClientType::FRAMED_COMPACT).to eq(0x03)
      expect(Thrift::HeaderClientType::UNFRAMED_COMPACT).to eq(0x04)
    end
  end

  describe Thrift::HeaderSubprotocolID do
    it "should define protocol ID constants" do
      expect(Thrift::HeaderSubprotocolID::BINARY).to eq(0x00)
      expect(Thrift::HeaderSubprotocolID::COMPACT).to eq(0x02)
    end
  end

  describe Thrift::HeaderTransformID do
    it "should define transform ID constants" do
      expect(Thrift::HeaderTransformID::ZLIB).to eq(0x01)
    end
  end

  describe Thrift::HeaderTransport do
    def header_frame(payload, headers = {})
      buffer = Thrift::MemoryBufferTransport.new
      writer = Thrift::HeaderTransport.new(buffer)
      headers.each { |key, value| writer.set_header(key, value) }
      writer.write(payload)
      writer.flush
      buffer.read(buffer.available)
    end

    def binary_message
      [Thrift::BinaryProtocol::VERSION_1 | Thrift::MessageTypes::CALL].pack('N')
    end

    def compact_message
      [0x82, 0x21, 0, 0].pack('C*')
    end

    def framed(message)
      [message.bytesize].pack('N') + message
    end

    def unframed_message(protocol_class, name = "legacy_unframed")
      buffer = Thrift::MemoryBufferTransport.new
      protocol = protocol_class.new(buffer)
      protocol.write_message_begin(name, Thrift::MessageTypes::CALL, 1)
      protocol.write_struct_begin("Args")
      protocol.write_field_stop
      protocol.write_struct_end
      protocol.write_message_end
      buffer.read(buffer.available)
    end

    def read_unframed_message(protocol)
      name = protocol.read_message_begin.first
      protocol.skip(Thrift::Types::STRUCT)
      protocol.read_message_end
      name
    end

    before(:each) do
      @underlying = Thrift::MemoryBufferTransport.new
      @trans = Thrift::HeaderTransport.new(@underlying)
    end

    it "should provide a to_s that describes the encapsulation" do
      expect(@trans.to_s).to eq("header(memory)")
    end

    it "should pass through open?/open/close" do
      mock_transport = double("Transport")
      expect(mock_transport).to receive(:open?).and_return(true)
      expect(mock_transport).to receive(:open).and_return(nil)
      expect(mock_transport).to receive(:close).and_return(nil)

      trans = Thrift::HeaderTransport.new(mock_transport)
      expect(trans.open?).to be true
      trans.open
      trans.close
    end

    describe "header management" do
      it "should allow setting and getting headers" do
        @trans.set_header("key1", "value1")
        @trans.set_header("key2", "value2")
        # Headers aren't read until we receive data, so write and read back
        expect(@trans.get_headers).to eq({})
      end

      it "should clear headers" do
        @trans.set_header("key1", "value1")
        @trans.clear_headers
        # Write and flush to verify headers were cleared
        @trans.write("test")
        @trans.flush
      end

      it "should add transforms" do
        expect { @trans.add_transform(Thrift::HeaderTransformID::ZLIB) }.not_to raise_error
      end

      it "should reject unknown transforms" do
        expect { @trans.add_transform(999) }.to raise_error(Thrift::TransportException)
      end
    end

    describe "write and flush" do
      it "should buffer writes" do
        @trans.write("hello")
        @trans.write(" world")
        expect(@underlying.available).to eq(0)
      end

      it "should write Header format on flush" do
        @trans.write("test payload")
        @trans.flush

        # Read back the frame
        data = @underlying.read(@underlying.available)

        # Should have frame length (4 bytes) + header + payload
        expect(data.bytesize).to be > 16

        # First 4 bytes are frame length
        frame_size = data[0, 4].unpack('N').first
        expect(frame_size).to eq(data.bytesize - 4)

        # Next 2 bytes should be header magic
        magic = data[4, 2].unpack('n').first
        expect(magic).to eq(Thrift::HeaderTransport::HEADER_MAGIC)
      end

      it "should include headers in frame" do
        @trans.set_header("test-key", "test-value")
        @trans.write("payload")
        @trans.flush

        # Read back and verify it's larger due to headers
        data = @underlying.read(@underlying.available)
        expect(data.bytesize).to be > 30 # Should include header key-value
      end

      it "should write the configured sequence id into the frame header" do
        @trans.sequence_id = 456
        @trans.write("payload")
        @trans.flush

        data = @underlying.read(@underlying.available)
        expect(data[8, 4].unpack('N').first).to eq(456)
      end

      it "should apply ZLIB transform" do
        @trans.add_transform(Thrift::HeaderTransformID::ZLIB)
        original_payload = "a" * 1000 # Compressible data
        @trans.write(original_payload)
        @trans.flush

        data = @underlying.read(@underlying.available)
        # Compressed frame should be smaller than uncompressed
        expect(data.bytesize).to be < original_payload.bytesize
      end
    end

    describe "frame size limits" do
      it "should reject payloads larger than max frame size" do
        @trans.set_max_frame_size(4)
        @trans.write("12345")
        expect { @trans.flush }.to raise_error(Thrift::TransportException, /frame that is too large/)
      end

      {
        "binary" => Thrift::BinaryProtocol,
        "compact" => Thrift::CompactProtocol
      }.each do |protocol_name, protocol_class|
        it "enforces max frame size for unframed #{protocol_name} messages" do
          payload = unframed_message(protocol_class)

          exact_limit = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(payload))
          exact_limit.set_max_frame_size(payload.bytesize)
          expect(read_unframed_message(Thrift::HeaderProtocol.new(exact_limit))).to eq("legacy_unframed")

          over_limit = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(payload))
          over_limit.set_max_frame_size(payload.bytesize - 1)
          protocol = Thrift::HeaderProtocol.new(over_limit)
          expect { read_unframed_message(protocol) }.to raise_error(
            Thrift::TransportException,
            "Unframed message size exceeds maximum #{payload.bytesize - 1}"
          ) do |error|
            expect(error.type).to eq(Thrift::TransportException::SIZE_LIMIT)
          end
        end
      end

      it "rejects an unframed protocol signature larger than the configured limit" do
        payload = unframed_message(Thrift::BinaryProtocol)
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(payload))
        read_trans.set_max_frame_size(3)

        expect { read_trans.read(1) }.to raise_error(
          Thrift::TransportException,
          "Unframed message size exceeds maximum 3"
        ) do |error|
          expect(error.type).to eq(Thrift::TransportException::SIZE_LIMIT)
        end
      end

      it "counts bytes actually returned by partial unframed reads" do
        payload = unframed_message(Thrift::BinaryProtocol)
        chunked_transport = Class.new(Thrift::BaseTransport) do
          def initialize(data)
            @data = data.dup
          end

          def read(size)
            @data.slice!(0, [size, 2].min)
          end
        end
        read_trans = Thrift::HeaderTransport.new(chunked_transport.new(payload))
        read_trans.set_max_frame_size(payload.bytesize)

        expect(read_unframed_message(Thrift::HeaderProtocol.new(read_trans))).to eq("legacy_unframed")
      end

      it "returns partial unframed data up to the configured limit" do
        payload = unframed_message(Thrift::BinaryProtocol)
        chunked_transport = Class.new(Thrift::BaseTransport) do
          def initialize(data)
            @data = data.dup
          end

          def read(size)
            @data.slice!(0, [size, 1].min)
          end
        end
        read_trans = Thrift::HeaderTransport.new(chunked_transport.new(payload))
        read_trans.set_max_frame_size(5)

        expect(read_trans.read(payload.bytesize)).to eq(payload.byteslice(0, 5))
        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException) do |error|
          expect(error.type).to eq(Thrift::TransportException::SIZE_LIMIT)
        end
      end

      it "returns the unframed protocol signature at the configured limit" do
        payload = unframed_message(Thrift::BinaryProtocol)
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(payload))
        read_trans.set_max_frame_size(4)

        expect(read_trans.read(payload.bytesize)).to eq(payload.byteslice(0, 4))
        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException) do |error|
          expect(error.type).to eq(Thrift::TransportException::SIZE_LIMIT)
        end
      end

      protocol_pairs = {
        "binary" => [Thrift::BinaryProtocol, Thrift::BinaryProtocol],
        "compact" => [Thrift::CompactProtocol, Thrift::CompactProtocol]
      }
      if defined?(Thrift::BinaryProtocolAccelerated)
        protocol_pairs["accelerated binary"] = [
          Thrift::BinaryProtocol,
          Thrift::BinaryProtocolAccelerated
        ]
      end

      protocol_pairs.each do |protocol_name, (writer_class, reader_class)|
        it "resets the unframed size budget between #{protocol_name} messages" do
          first = unframed_message(writer_class, "first")
          second = unframed_message(writer_class, "second")
          read_trans = Thrift::HeaderTransport.new(
            Thrift::MemoryBufferTransport.new(first + second)
          )
          read_trans.set_max_frame_size([first.bytesize, second.bytesize].max)
          expect(read_trans).to receive(:message_boundaries?).once.and_return(true)
          protocol = reader_class.new(read_trans)

          expect(read_unframed_message(protocol)).to eq("first")
          expect(read_unframed_message(protocol)).to eq("second")
        end
      end
    end

    describe "decompressed size limits" do
      it "should accept a valid max_decompressed_size" do
        expect { @trans.set_max_decompressed_size(1024) }.not_to raise_error
        expect { @trans.set_max_decompressed_size(Thrift::HeaderTransport::MAX_FRAME_SIZE) }.not_to raise_error
      end

      it "should reject invalid max_decompressed_size" do
        expect { @trans.set_max_decompressed_size(0) }.to raise_error(ArgumentError)
        expect { @trans.set_max_decompressed_size(-1) }.to raise_error(ArgumentError)
        expect { @trans.set_max_decompressed_size(Thrift::HeaderTransport::MAX_FRAME_SIZE + 1) }.to raise_error(ArgumentError)
      end

      it "should decompress ZLIB payload within the limit" do
        write_buf = Thrift::MemoryBufferTransport.new
        writer = Thrift::HeaderTransport.new(write_buf)
        writer.add_transform(Thrift::HeaderTransformID::ZLIB)
        writer.write("A" * 1_000)
        writer.flush

        written_data = write_buf.read(write_buf.available)
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(written_data))
        read_trans.set_max_decompressed_size(2_000)

        expect(read_trans.read(1_000)).to eq("A" * 1_000)
      end

      it "should raise SIZE_LIMIT TransportException when decompressed output exceeds the limit" do
        write_buf = Thrift::MemoryBufferTransport.new
        writer = Thrift::HeaderTransport.new(write_buf)
        writer.add_transform(Thrift::HeaderTransformID::ZLIB)
        writer.write("A" * 10_000)
        writer.flush

        written_data = write_buf.read(write_buf.available)
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(written_data))
        read_trans.set_max_decompressed_size(100)

        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException) do |e|
          expect(e.type).to eq(Thrift::TransportException::SIZE_LIMIT)
          expect(e.message).to match(/limit/)
        end
      end

      it "should stream oversized ZLIB output before enforcing the limit" do
        write_buf = Thrift::MemoryBufferTransport.new
        writer = Thrift::HeaderTransport.new(write_buf)
        writer.add_transform(Thrift::HeaderTransformID::ZLIB)
        writer.write("A" * 8_000_000)
        writer.flush

        written_data = write_buf.read(write_buf.available)
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(written_data))
        read_trans.set_max_decompressed_size(100)

        expect(Zlib::Inflate).to receive(:new).and_wrap_original do |new, *args|
          inflater = new.call(*args)
          expect(inflater).to receive(:inflate).and_wrap_original do |inflate, compressed, &block|
            expect(block).not_to be_nil
            inflate.call(compressed, &block)
          end
          inflater
        end

        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException) do |e|
          expect(e.type).to eq(Thrift::TransportException::SIZE_LIMIT)
        end
      end
    end

    describe "read and frame detection" do
      it "should detect Header format" do
        # Write a Header frame
        @trans.write("test data")
        @trans.flush

        # Reset for reading
        written_data = @underlying.read(@underlying.available)
        read_transport = Thrift::MemoryBufferTransport.new(written_data)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        result = read_trans.read(9)
        expect(result).to eq("test data")
      end

      it "should detect framed binary protocol" do
        # Create a framed binary message
        payload = [Thrift::BinaryProtocol::VERSION_1 | Thrift::MessageTypes::CALL].pack('N')
        payload << "test"
        frame = [payload.bytesize].pack('N') + payload

        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        result = read_trans.read(payload.bytesize)
        expect(result).to eq(payload)
      end

      it "should detect unframed binary protocol" do
        # Create an unframed binary message (version word first)
        message = [Thrift::BinaryProtocol::VERSION_1 | Thrift::MessageTypes::CALL].pack('N')
        message << "test"

        read_transport = Thrift::MemoryBufferTransport.new(message)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        result = read_trans.read(message.bytesize)
        expect(result).to eq(message)
      end

      it "should read headers from Header frame" do
        # Write with headers
        @trans.set_header("request-id", "12345")
        @trans.write("payload")
        @trans.flush

        # Read back
        written_data = @underlying.read(@underlying.available)
        read_transport = Thrift::MemoryBufferTransport.new(written_data)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        read_trans.read(7)
        headers = read_trans.get_headers
        expect(headers["request-id"]).to eq("12345")
      end

      {
        "framed binary" => [:binary_message, true],
        "unframed binary" => [:binary_message, false],
        "framed compact" => [:compact_message, true],
        "unframed compact" => [:compact_message, false]
      }.each do |legacy_name, (legacy_message, is_framed)|
        it "does not carry Header metadata through a #{legacy_name} protocol switch" do
          legacy_payload = public_send(legacy_message)
          bytes = header_frame("A", "request-id" => "first")
          bytes << (is_framed ? framed(legacy_payload) : legacy_payload)
          bytes << header_frame("B", "request-id" => "second")
          read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(bytes))

          expect(read_trans.read(1)).to eq("A")
          expect(read_trans.get_headers).to eq("request-id" => "first")

          read_trans.reset_protocol
          expect(read_trans.read(4)).to eq(legacy_payload)
          expect(read_trans.get_headers).to eq({})

          read_trans.reset_protocol
          expect(read_trans.read(1)).to eq("B")
          expect(read_trans.get_headers).to eq("request-id" => "second")
        end
      end

      it "keeps metadata empty across multiple legacy frames" do
        bytes = header_frame("A", "request-id" => "first")
        bytes << framed(binary_message)
        bytes << framed(binary_message)
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(bytes))

        expect(read_trans.read(1)).to eq("A")
        expect(read_trans.get_headers).to eq("request-id" => "first")

        2.times do
          read_trans.reset_protocol
          expect(read_trans.read(4)).to eq(binary_message)
          expect(read_trans.get_headers).to eq({})
        end
      end

      it "clears metadata before reporting a malformed following frame" do
        malformed_frame = [4].pack('N') + "nope"
        bytes = header_frame("A", "request-id" => "first") + malformed_frame
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(bytes))

        expect(read_trans.read(1)).to eq("A")
        expect(read_trans.get_headers).to eq("request-id" => "first")

        expect { read_trans.reset_protocol }.to raise_error(
          Thrift::TransportException,
          "Could not detect client transport type"
        )
        expect(read_trans.get_headers).to eq({})
      end

      it "should decode signed sequence ids from Header frames" do
        @trans.sequence_id = -2147483648
        @trans.write("payload")
        @trans.flush

        written_data = @underlying.read(@underlying.available)
        read_transport = Thrift::MemoryBufferTransport.new(written_data)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        expect(read_trans.read(7)).to eq("payload")
        expect(read_trans.sequence_id).to eq(-2147483648)
      end

      it "should decompress ZLIB payload" do
        # Write with ZLIB
        @trans.add_transform(Thrift::HeaderTransformID::ZLIB)
        original = "hello world this is a test"
        @trans.write(original)
        @trans.flush

        # Read back
        written_data = @underlying.read(@underlying.available)
        read_transport = Thrift::MemoryBufferTransport.new(written_data)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        result = read_trans.read(original.bytesize)
        expect(result).to eq(original)
      end
    end

    describe "header parsing protections" do
      it "rejects frame sizes shorter than a protocol signature" do
        (0..3).each do |frame_size|
          frame = [frame_size].pack('N') + ("\x00" * frame_size)
          read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(frame))

          expect { read_trans.read(1) }.to raise_error(
            Thrift::TransportException,
            "Frame size #{frame_size} is too small"
          ) do |error|
            expect(error.type).to eq(Thrift::TransportException::UNKNOWN)
          end
        end
      end

      it "reports EOF when the frame size is fragmented" do
        (0..3).each do |available_size|
          read_trans = Thrift::HeaderTransport.new(
            Thrift::MemoryBufferTransport.new("\x00" * available_size)
          )

          expect { read_trans.read(1) }.to raise_error(
            Thrift::TransportException,
            "Unexpected EOF reading frame size"
          ) do |error|
            expect(error.type).to eq(Thrift::TransportException::END_OF_FILE)
          end
        end
      end

      it "reports EOF when the declared frame is fragmented" do
        frame = [4].pack('N') + "\x80\x01\x00".b
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new(frame))

        expect { read_trans.read(1) }.to raise_error(
          Thrift::TransportException,
          "Unexpected EOF reading frame"
        ) do |error|
          expect(error.type).to eq(Thrift::TransportException::END_OF_FILE)
        end
      end

      it "accepts a four-byte framed binary protocol signature" do
        payload = [Thrift::BinaryProtocol::VERSION_1 | Thrift::MessageTypes::CALL].pack('N')
        read_trans = Thrift::HeaderTransport.new(Thrift::MemoryBufferTransport.new([payload.bytesize].pack('N') + payload))

        expect(read_trans.read(payload.bytesize)).to eq(payload)
      end

      it "should reject unreasonable header sizes" do
        frame = build_header_frame("", Thrift::Bytes.empty_byte_buffer, header_words: 16_384)
        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException, /Header size is unreasonable/)
      end

      it "should reject header frames that are too small" do
        frame = Thrift::Bytes.empty_byte_buffer
        frame << [9].pack('N')
        frame << [Thrift::HeaderTransport::HEADER_MAGIC].pack('n')
        frame << [0].pack('n')
        frame << [0].pack('N')
        frame << [0].pack('n')
        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException, /frame is too small/)
      end

      it "should reject varints that cross header boundary" do
        header_data = [0x80, 0x80, 0x80, 0x80].pack('C*')
        frame = build_header_frame(header_data)
        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException, /header boundary/)
      end

      it "should reject varints longer than uint32" do
        header_data = "\x80".b * 65_532
        frame = build_header_frame(header_data)
        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException, /over 5 bytes/)
      end

      it "should reject uint32 varints with an overflowing fifth byte" do
        header_data = ([0x80] * 4 + [0x10]).pack('C*')
        frame = build_header_frame(header_data)
        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException, /overflows uint32/)
      end

      it "should accept uint32 varints with a valid fifth byte" do
        header_data = ([0x80] * 4 + [0x0f, 0]).pack('C*')
        frame = build_header_frame(header_data)
        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        expect(read_trans.read(1)).to eq(Thrift::Bytes.empty_byte_buffer)
        expect(read_trans.protocol_id).to eq(0xf0000000)
      end

      it "should reject strings that exceed header boundary" do
        header_data = +""
        header_data << varint32(Thrift::HeaderSubprotocolID::BINARY)
        header_data << varint32(0)
        header_data << varint32(Thrift::HeaderInfoType::KEY_VALUE)
        header_data << varint32(1)
        header_data << varint32(10)
        header_data << "a"

        frame = build_header_frame(header_data)
        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        expect { read_trans.read(1) }.to raise_error(Thrift::TransportException, /Info header length exceeds header size/)
      end
    end

    describe "round-trip" do
      it "should handle complete write-read cycle" do
        # Write
        @trans.set_header("trace-id", "abc123")
        @trans.write("hello world")
        @trans.flush

        # Read
        written_data = @underlying.read(@underlying.available)
        read_transport = Thrift::MemoryBufferTransport.new(written_data)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        result = read_trans.read(11)
        expect(result).to eq("hello world")
        expect(read_trans.get_headers["trace-id"]).to eq("abc123")
      end

      it "should handle multiple headers" do
        @trans.set_header("header1", "value1")
        @trans.set_header("header2", "value2")
        @trans.set_header("header3", "value3")
        @trans.write("data")
        @trans.flush

        written_data = @underlying.read(@underlying.available)
        read_transport = Thrift::MemoryBufferTransport.new(written_data)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        read_trans.read(4)
        headers = read_trans.get_headers
        expect(headers["header1"]).to eq("value1")
        expect(headers["header2"]).to eq("value2")
        expect(headers["header3"]).to eq("value3")
      end

      it "should handle ZLIB compression round-trip" do
        @trans.add_transform(Thrift::HeaderTransformID::ZLIB)
        @trans.set_header("compressed", "true")
        original = "x" * 500
        @trans.write(original)
        @trans.flush

        written_data = @underlying.read(@underlying.available)
        read_transport = Thrift::MemoryBufferTransport.new(written_data)
        read_trans = Thrift::HeaderTransport.new(read_transport)

        result = read_trans.read(500)
        expect(result).to eq(original)
        expect(read_trans.get_headers["compressed"]).to eq("true")
      end
    end

    describe "client type restrictions" do
      it "should reject disallowed client types" do
        # Only allow HEADERS
        allowed = [Thrift::HeaderClientType::HEADERS]

        # Create framed binary message
        payload = [Thrift::BinaryProtocol::VERSION_1 | Thrift::MessageTypes::CALL].pack('N')
        frame = [payload.bytesize].pack('N') + payload

        read_transport = Thrift::MemoryBufferTransport.new(frame)
        read_trans = Thrift::HeaderTransport.new(read_transport, allowed)

        expect { read_trans.read(4) }.to raise_error(Thrift::TransportException)
      end
    end
  end

  describe Thrift::HeaderTransportFactory do
    it "should wrap transport in HeaderTransport" do
      mock_transport = double("Transport")
      factory = Thrift::HeaderTransportFactory.new
      result = factory.get_transport(mock_transport)
      expect(result).to be_a(Thrift::HeaderTransport)
    end

    it "should provide a reasonable to_s" do
      expect(Thrift::HeaderTransportFactory.new.to_s).to eq("header")
    end

    it "should pass allowed_client_types to transport" do
      allowed = [Thrift::HeaderClientType::HEADERS]
      factory = Thrift::HeaderTransportFactory.new(allowed)

      mock_transport = Thrift::MemoryBufferTransport.new
      result = factory.get_transport(mock_transport)

      expect(result).to be_a(Thrift::HeaderTransport)
    end
  end
end
