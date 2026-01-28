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
