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

module Thrift
  # HeaderProtocol is a protocol that wraps HeaderTransport and delegates
  # to either BinaryProtocol or CompactProtocol based on auto-detection.
  #
  # It provides access to header management (get_headers, set_header, etc.)
  # through the underlying HeaderTransport.
  #
  # Example usage:
  #   socket = Thrift::Socket.new('localhost', 9090)
  #   protocol = Thrift::HeaderProtocol.new(socket)
  #   client = MyService::Client.new(protocol)
  #   protocol.trans.open
  #   client.some_method()
  #   protocol.trans.close
  #
  class HeaderProtocol < BaseProtocol
    # Creates a new HeaderProtocol.
    #
    # @param transport [BaseTransport, HeaderTransport] The transport to wrap.
    #   If not already a HeaderTransport, it will be wrapped in one.
    # @param allowed_client_types [Array<Integer>] Allowed client types for auto-detection
    # @param default_protocol [Integer] Default protocol ID (BINARY or COMPACT)
    def initialize(transport, allowed_client_types = nil, default_protocol = HeaderSubprotocolID::COMPACT)
      # Wrap transport in HeaderTransport if not already wrapped
      if transport.is_a?(HeaderTransport)
        @header_transport = transport
      else
        @header_transport = HeaderTransport.new(transport, allowed_client_types, default_protocol)
      end

      @default_protocol = default_protocol
      @current_protocol_id = default_protocol

      # Create initial protocol
      @protocol = create_protocol(@current_protocol_id)
    end

    # Returns the HeaderTransport
    def trans
      @header_transport
    end

    # Returns headers read from the last message
    def get_headers
      @header_transport.get_headers
    end

    # Sets a header to be sent with the next message
    def set_header(key, value)
      @header_transport.set_header(key, value)
    end

    # Clears all write headers
    def clear_headers
      @header_transport.clear_headers
    end

    # Adds a transform (e.g., ZLIB compression)
    def add_transform(transform_id)
      @header_transport.add_transform(transform_id)
    end

    # Write methods - delegate to underlying protocol
    def write_message_begin(name, type, seqid)
      @protocol.write_message_begin(name, type, seqid)
    end

    def write_message_end
      @protocol.write_message_end
    end

    def write_struct_begin(name)
      @protocol.write_struct_begin(name)
    end

    def write_struct_end
      @protocol.write_struct_end
    end

    def write_field_begin(name, type, id)
      @protocol.write_field_begin(name, type, id)
    end

    def write_field_end
      @protocol.write_field_end
    end

    def write_field_stop
      @protocol.write_field_stop
    end

    def write_map_begin(ktype, vtype, size)
      @protocol.write_map_begin(ktype, vtype, size)
    end

    def write_map_end
      @protocol.write_map_end
    end

    def write_list_begin(etype, size)
      @protocol.write_list_begin(etype, size)
    end

    def write_list_end
      @protocol.write_list_end
    end

    def write_set_begin(etype, size)
      @protocol.write_set_begin(etype, size)
    end

    def write_set_end
      @protocol.write_set_end
    end

    def write_bool(bool)
      @protocol.write_bool(bool)
    end

    def write_byte(byte)
      @protocol.write_byte(byte)
    end

    def write_i16(i16)
      @protocol.write_i16(i16)
    end

    def write_i32(i32)
      @protocol.write_i32(i32)
    end

    def write_i64(i64)
      @protocol.write_i64(i64)
    end

    def write_double(dub)
      @protocol.write_double(dub)
    end

    def write_string(str)
      @protocol.write_string(str)
    end

    def write_binary(buf)
      @protocol.write_binary(buf)
    end

    def write_uuid(uuid)
      @protocol.write_uuid(uuid)
    end

    # Read methods - delegate to underlying protocol
    # read_message_begin handles protocol switching after detection
    def read_message_begin
      begin
        @header_transport.reset_protocol
        reset_protocol_if_needed
      rescue ProtocolException => ex
        app_ex = ApplicationException.new(ApplicationException::INVALID_PROTOCOL, ex.message)
        write_message_begin("", MessageTypes::EXCEPTION, 0)
        app_ex.write(self)
        write_message_end
        @header_transport.flush
        raise ex
      end
      @protocol.read_message_begin
    end

    def read_message_end
      @protocol.read_message_end
    end

    def read_struct_begin
      @protocol.read_struct_begin
    end

    def read_struct_end
      @protocol.read_struct_end
    end

    def read_field_begin
      @protocol.read_field_begin
    end

    def read_field_end
      @protocol.read_field_end
    end

    def read_map_begin
      @protocol.read_map_begin
    end

    def read_map_end
      @protocol.read_map_end
    end

    def read_list_begin
      @protocol.read_list_begin
    end

    def read_list_end
      @protocol.read_list_end
    end

    def read_set_begin
      @protocol.read_set_begin
    end

    def read_set_end
      @protocol.read_set_end
    end

    def read_bool
      @protocol.read_bool
    end

    def read_byte
      @protocol.read_byte
    end

    def read_i16
      @protocol.read_i16
    end

    def read_i32
      @protocol.read_i32
    end

    def read_i64
      @protocol.read_i64
    end

    def read_double
      @protocol.read_double
    end

    def read_string
      @protocol.read_string
    end

    def read_binary
      @protocol.read_binary
    end

    def read_uuid
      @protocol.read_uuid
    end

    def to_s
      "header(#{@protocol.to_s})"
    end

    private

    # Checks if the protocol needs to be switched after reading
    def reset_protocol_if_needed
      new_protocol_id = @header_transport.protocol_id
      if new_protocol_id != @current_protocol_id
        @protocol = create_protocol(new_protocol_id)
        @current_protocol_id = new_protocol_id
      end
    end

    # Creates a protocol instance based on protocol ID
    def create_protocol(protocol_id)
      case protocol_id
      when HeaderSubprotocolID::BINARY
        BinaryProtocol.new(@header_transport)
      when HeaderSubprotocolID::COMPACT
        CompactProtocol.new(@header_transport)
      else
        raise ProtocolException.new(
          ProtocolException::INVALID_DATA,
          "Unknown protocol ID: #{protocol_id}"
        )
      end
    end
  end

  # Factory for creating HeaderProtocol instances
  class HeaderProtocolFactory < BaseProtocolFactory
    # Creates a new HeaderProtocolFactory.
    #
    # @param allowed_client_types [Array<Integer>] Allowed client types for auto-detection
    # @param default_protocol [Integer] Default protocol ID (BINARY or COMPACT)
    def initialize(allowed_client_types = nil, default_protocol = HeaderSubprotocolID::BINARY)
      @allowed_client_types = allowed_client_types
      @default_protocol = default_protocol
    end

    def get_protocol(trans)
      HeaderProtocol.new(trans, @allowed_client_types, @default_protocol)
    end

    def to_s
      "header"
    end
  end
end
