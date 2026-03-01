# encoding: ascii-8bit
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

require 'stringio'
require 'zlib'

module Thrift
  # Client type constants for Header protocol
  module HeaderClientType
    HEADERS = 0x00
    FRAMED_BINARY = 0x01
    UNFRAMED_BINARY = 0x02
    FRAMED_COMPACT = 0x03
    UNFRAMED_COMPACT = 0x04
  end

  # Subprotocol ID constants for Header transport
  module HeaderSubprotocolID
    BINARY = 0x00
    COMPACT = 0x02
  end

  # Transform ID constants for Header transport
  module HeaderTransformID
    ZLIB = 0x01
  end

  # Info header type constants
  module HeaderInfoType
    KEY_VALUE = 0x01
  end

  # HeaderTransport implements the THeader framing protocol.
  #
  # THeader is a transport that adds headers and supports multiple protocols
  # and transforms. It can auto-detect and communicate with legacy protocols
  # (framed/unframed binary/compact) for backward compatibility.
  #
  # Wire format:
  #   +----------------------------------------------------------------+
  #   | LENGTH (4 bytes, big-endian, excludes itself)                  |
  #   +----------------------------------------------------------------+
  #   | HEADER MAGIC (2 bytes: 0x0FFF) | FLAGS (2 bytes)               |
  #   +----------------------------------------------------------------+
  #   | SEQUENCE NUMBER (4 bytes)                                      |
  #   +----------------------------------------------------------------+
  #   | HEADER SIZE/4 (2 bytes)        | HEADER DATA (variable)...    |
  #   +----------------------------------------------------------------+
  #   | PAYLOAD (variable)                                             |
  #   +----------------------------------------------------------------+
  #
  class HeaderTransport < BaseTransport
    # Header magic value (first 2 bytes of header)
    HEADER_MAGIC = 0x0FFF

    # Maximum frame size (~1GB)
    MAX_FRAME_SIZE = 0x3FFFFFFF

    # Binary protocol version mask and version 1
    BINARY_VERSION_MASK = 0xffff0000
    BINARY_VERSION_1 = 0x80010000

    # Compact protocol ID
    COMPACT_PROTOCOL_ID = 0x82
    COMPACT_VERSION_MASK = 0x1f
    COMPACT_VERSION = 0x01

    attr_reader :protocol_id, :sequence_id, :flags

    # Creates a new HeaderTransport wrapping the given transport.
    #
    # @param transport [BaseTransport] The underlying transport to wrap
    # @param allowed_client_types [Array<Integer>] Allowed client types for auto-detection.
    #   Defaults to all types for backward compatibility.
    # @param default_protocol [Integer] Default protocol ID (BINARY or COMPACT)
    def initialize(transport, allowed_client_types = nil, default_protocol = HeaderSubprotocolID::COMPACT)
      @transport = transport
      @client_type = HeaderClientType::HEADERS
      @protocol_id = default_protocol
      @allowed_client_types = allowed_client_types || [
        HeaderClientType::HEADERS,
        HeaderClientType::FRAMED_BINARY,
        HeaderClientType::UNFRAMED_BINARY,
        HeaderClientType::FRAMED_COMPACT,
        HeaderClientType::UNFRAMED_COMPACT
      ]

      @read_buffer = StringIO.new(Bytes.empty_byte_buffer)
      @write_buffer = StringIO.new(Bytes.empty_byte_buffer)

      @read_headers = {}
      @write_headers = {}
      @write_transforms = []

      @sequence_id = 0
      @flags = 0
      @max_frame_size = MAX_FRAME_SIZE
    end

    def open?
      @transport.open?
    end

    def open
      @transport.open
    end

    def close
      @transport.close
    end

    # Returns the headers read from the last frame
    def get_headers
      @read_headers
    end

    # Sets a header to be written with the next flush
    #
    # @param key [String] Header key (must be binary string)
    # @param value [String] Header value (must be binary string)
    def set_header(key, value)
      key = Bytes.force_binary_encoding(key.to_s)
      value = Bytes.force_binary_encoding(value.to_s)
      @write_headers[key] = value
    end

    # Clears all write headers
    def clear_headers
      @write_headers.clear
    end

    # Adds a transform to apply when writing
    #
    # @param transform_id [Integer] Transform ID (e.g., HeaderTransformID::ZLIB)
    def add_transform(transform_id)
      unless transform_id == HeaderTransformID::ZLIB
        raise TransportException.new(TransportException::UNKNOWN, "Unknown transform: #{transform_id}")
      end
      @write_transforms << transform_id unless @write_transforms.include?(transform_id)
    end

    # Sets the maximum allowed frame size
    def set_max_frame_size(size)
      if size <= 0 || size > MAX_FRAME_SIZE
        raise ArgumentError, "max_frame_size must be > 0 and <= #{MAX_FRAME_SIZE}"
      end
      @max_frame_size = size
    end

    def read(sz)
      # Try reading from existing buffer
      data = @read_buffer.read(sz)
      data = Bytes.empty_byte_buffer if data.nil?

      bytes_left = sz - data.bytesize
      return data if bytes_left == 0

      # Handle unframed passthrough - read directly from underlying transport
      if @client_type == HeaderClientType::UNFRAMED_BINARY ||
         @client_type == HeaderClientType::UNFRAMED_COMPACT
        return data + @transport.read(bytes_left)
      end

      # Need to read the next frame
      read_frame(bytes_left)
      additional = @read_buffer.read(bytes_left)
      data + (additional || Bytes.empty_byte_buffer)
    end

    def write(buf)
      @write_buffer.write(Bytes.force_binary_encoding(buf))
    end

    def flush
      payload = @write_buffer.string
      @write_buffer = StringIO.new(Bytes.empty_byte_buffer)

      return if payload.empty?
      if payload.bytesize > @max_frame_size
        raise TransportException.new(TransportException::UNKNOWN, "Attempting to send frame that is too large")
      end

      case @client_type
      when HeaderClientType::HEADERS
        flush_header_format(payload)
      when HeaderClientType::FRAMED_BINARY, HeaderClientType::FRAMED_COMPACT
        flush_framed(payload)
      when HeaderClientType::UNFRAMED_BINARY, HeaderClientType::UNFRAMED_COMPACT
        @transport.write(payload)
        @transport.flush
      else
        flush_header_format(payload)
      end
    end

    def to_s
      "header(#{@transport.to_s})"
    end

    # Reads the next frame to detect protocol/client type before decoding.
    def reset_protocol
      return unless @read_buffer.nil? || @read_buffer.eof?

      read_frame(0)
    end

    private

    # Sets the client type after validation
    def set_client_type(client_type)
      unless @allowed_client_types.include?(client_type)
        raise TransportException.new(TransportException::UNKNOWN, "Client type #{client_type} not allowed by server")
      end
      @client_type = client_type
    end

    # Reads the next frame, detecting client type on first read
    def read_frame(req_sz)
      # Read first 4 bytes - could be frame length or protocol magic
      first_word = @transport.read_all(4)
      frame_size = first_word.unpack('N').first

      # Check for unframed binary protocol
      if (frame_size & BINARY_VERSION_MASK) == BINARY_VERSION_1
        set_client_type(HeaderClientType::UNFRAMED_BINARY)
        @protocol_id = HeaderSubprotocolID::BINARY
        handle_unframed(first_word, req_sz)
        return
      end

      # Check for unframed compact protocol
      if Bytes.get_string_byte(first_word, 0) == COMPACT_PROTOCOL_ID &&
         (Bytes.get_string_byte(first_word, 1) & COMPACT_VERSION_MASK) == COMPACT_VERSION
        set_client_type(HeaderClientType::UNFRAMED_COMPACT)
        @protocol_id = HeaderSubprotocolID::COMPACT
        handle_unframed(first_word, req_sz)
        return
      end

      # It's a framed protocol - validate frame size
      if frame_size > @max_frame_size
        raise TransportException.new(TransportException::UNKNOWN, "Frame size #{frame_size} exceeds maximum #{@max_frame_size}")
      end

      # Read the complete frame
      frame_data = @transport.read_all(frame_size)
      frame_buf = StringIO.new(frame_data)

      # Check the second word for protocol type
      second_word = frame_buf.read(4)
      frame_buf.rewind

      magic = second_word.unpack('n').first

      if magic == HEADER_MAGIC
        if frame_size < 10
          raise TransportException.new(TransportException::UNKNOWN, "Header transport frame is too small")
        end
        set_client_type(HeaderClientType::HEADERS)
        @read_buffer = parse_header_format(frame_buf)
      elsif (second_word.unpack('N').first & BINARY_VERSION_MASK) == BINARY_VERSION_1
        set_client_type(HeaderClientType::FRAMED_BINARY)
        @protocol_id = HeaderSubprotocolID::BINARY
        @read_buffer = frame_buf
      elsif Bytes.get_string_byte(second_word, 0) == COMPACT_PROTOCOL_ID &&
            (Bytes.get_string_byte(second_word, 1) & COMPACT_VERSION_MASK) == COMPACT_VERSION
        set_client_type(HeaderClientType::FRAMED_COMPACT)
        @protocol_id = HeaderSubprotocolID::COMPACT
        @read_buffer = frame_buf
      else
        raise TransportException.new(TransportException::UNKNOWN, "Could not detect client transport type")
      end
    end

    # Handles unframed protocol - puts first_word back in buffer
    def handle_unframed(first_word, req_sz)
      bytes_left = req_sz - 4
      if bytes_left > 0
        rest = @transport.read(bytes_left)
        @read_buffer = StringIO.new(first_word + rest)
      else
        @read_buffer = StringIO.new(first_word)
      end
    end

    # Parses a Header format frame
    def parse_header_format(buf)
      # Skip magic (already identified)
      buf.read(2)

      # Read flags and sequence ID
      @flags = buf.read(2).unpack('n').first
      @sequence_id = buf.read(4).unpack('N').first

      # Read header length (in 32-bit words)
      header_words = buf.read(2).unpack('n').first
      if header_words >= 16_384
        raise TransportException.new(TransportException::UNKNOWN, "Header size is unreasonable")
      end
      header_length = header_words * 4
      end_of_headers = buf.pos + header_length

      if end_of_headers > buf.string.bytesize
        raise TransportException.new(TransportException::UNKNOWN, "Header size exceeds frame size")
      end

      # Read protocol ID
      @protocol_id = read_varint32(buf, end_of_headers)

      # Read transforms
      transforms = []
      transform_count = read_varint32(buf, end_of_headers)
      transform_count.times do
        transform_id = read_varint32(buf, end_of_headers)
        unless transform_id == HeaderTransformID::ZLIB
          raise TransportException.new(TransportException::UNKNOWN, "Unknown transform: #{transform_id}")
        end
        transforms << transform_id
      end
      # Read info headers
      @read_headers = {}
      while buf.pos < end_of_headers
        info_type = read_varint32(buf, end_of_headers)
        if info_type == 0
          # header padding
          break
        elsif info_type == HeaderInfoType::KEY_VALUE
          count = read_varint32(buf, end_of_headers)
          count.times do
            key = read_varstring(buf, end_of_headers)
            value = read_varstring(buf, end_of_headers)
            @read_headers[key] = value
          end
        else
          # Unknown info type, skip to end of headers
          break
        end
      end

      # Skip any remaining header padding
      buf.pos = end_of_headers

      # Read payload and apply transforms
      payload = buf.read
      transforms.each do |transform_id|
        if transform_id == HeaderTransformID::ZLIB
          payload = Zlib::Inflate.inflate(payload)
        end
      end

      StringIO.new(payload)
    end

    # Flushes data in Header format
    def flush_header_format(payload)
      # Apply transforms
      @write_transforms.each do |transform_id|
        if transform_id == HeaderTransformID::ZLIB
          payload = Zlib::Deflate.deflate(payload)
        end
      end

      # Build header data
      header_buf = StringIO.new(Bytes.empty_byte_buffer)

      # Protocol ID
      write_varint32(header_buf, @protocol_id)

      # Transforms
      write_varint32(header_buf, @write_transforms.size)
      @write_transforms.each { |t| write_varint32(header_buf, t) }

      # Info headers (key-value pairs)
      unless @write_headers.empty?
        write_varint32(header_buf, HeaderInfoType::KEY_VALUE)
        write_varint32(header_buf, @write_headers.size)
        @write_headers.each do |key, value|
          write_varstring(header_buf, key)
          write_varstring(header_buf, value)
        end
        @write_headers = {}
      end

      # Pad header to 4-byte boundary
      header_data = header_buf.string
      padding = (4 - (header_data.bytesize % 4)) % 4
      header_data += "\x00" * padding

      # Calculate total frame size (excludes the 4-byte length field itself)
      # Frame = magic(2) + flags(2) + seqid(4) + header_len(2) + header_data + payload
      frame_size = 2 + 2 + 4 + 2 + header_data.bytesize + payload.bytesize

      # Write complete frame
      frame = Bytes.empty_byte_buffer
      frame << [frame_size].pack('N')               # Length
      frame << [HEADER_MAGIC].pack('n')             # Magic
      frame << [@flags].pack('n')                   # Flags
      frame << [@sequence_id].pack('N')             # Sequence ID
      frame << [header_data.bytesize / 4].pack('n') # Header length (in 32-bit words)
      frame << header_data                          # Header data
      frame << payload                              # Payload

      @transport.write(frame)
      @transport.flush
    end

    # Flushes data in simple framed format (for legacy compatibility)
    def flush_framed(payload)
      frame = [payload.bytesize].pack('N') + payload
      @transport.write(frame)
      @transport.flush
    end

    # Reads a varint32 from the given IO
    def read_varint32(io, boundary_pos = nil)
      shift = 0
      result = 0
      loop do
        if boundary_pos && io.pos >= boundary_pos
          raise TransportException.new(TransportException::UNKNOWN, "Trying to read past header boundary")
        end
        byte = io.getbyte
        raise TransportException.new(TransportException::END_OF_FILE, "Unexpected EOF reading varint") if byte.nil?
        result |= (byte & 0x7f) << shift
        break if (byte & 0x80) == 0
        shift += 7
      end
      result
    end

    # Writes a varint32 to the given IO
    def write_varint32(io, n)
      loop do
        if (n & ~0x7F) == 0
          io.write([n].pack('C'))
          break
        else
          io.write([(n & 0x7F) | 0x80].pack('C'))
          n >>= 7
        end
      end
    end

    # Reads a varint-prefixed string from the given IO
    def read_varstring(io, boundary_pos = nil)
      size = read_varint32(io, boundary_pos)
      if size < 0
        raise TransportException.new(TransportException::UNKNOWN, "Negative string size: #{size}")
      end
      if boundary_pos && size > (boundary_pos - io.pos)
        raise TransportException.new(TransportException::UNKNOWN, "Info header length exceeds header size")
      end
      data = io.read(size)
      if data.nil? || data.bytesize < size
        raise TransportException.new(TransportException::END_OF_FILE, "Unexpected EOF reading string")
      end
      data
    end

    # Writes a varint-prefixed string to the given IO
    def write_varstring(io, value)
      value = Bytes.force_binary_encoding(value)
      write_varint32(io, value.bytesize)
      io.write(value)
    end
  end

  # Factory for creating HeaderTransport instances
  class HeaderTransportFactory < BaseTransportFactory
    def initialize(allowed_client_types = nil, default_protocol = HeaderSubprotocolID::BINARY)
      @allowed_client_types = allowed_client_types
      @default_protocol = default_protocol
    end

    def get_transport(transport)
      HeaderTransport.new(transport, @allowed_client_types, @default_protocol)
    end

    def to_s
      "header"
    end
  end
end
