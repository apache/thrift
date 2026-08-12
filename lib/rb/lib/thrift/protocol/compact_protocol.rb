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

module Thrift
  class CompactProtocol < BaseProtocol

    PROTOCOL_ID = [0x82].pack('c').unpack('c').first
    VERSION = 1
    VERSION_MASK = 0x1f
    TYPE_MASK = 0xE0
    TYPE_BITS = 0x07
    TYPE_SHIFT_AMOUNT = 5
    MAX_VARINT32_BYTES = 5  # ceil(32/7); matches protobuf wire format
    VARINT32_PREFIX_BYTES = MAX_VARINT32_BYTES - 1
    MAX_VARINT32_LAST_BYTE = 0x0f
    MAX_VARINT_BYTES = 10   # ceil(64/7); matches protobuf wire format
    BYTE_MIN = -(2**7)
    BYTE_MAX = (2**7) - 1
    I16_MIN = -(2**15)
    I16_MAX = (2**15) - 1
    I32_MIN = -(2**31)
    I32_MAX = (2**31) - 1
    I64_MIN = -(2**63)
    I64_MAX = (2**63) - 1

    TSTOP = [nil, Types::STOP, 0]

    #
    # All of the on-wire type codes.
    #
    class CompactTypes
      BOOLEAN_TRUE   = 0x01
      BOOLEAN_FALSE  = 0x02
      BYTE           = 0x03
      I16            = 0x04
      I32            = 0x05
      I64            = 0x06
      DOUBLE         = 0x07
      BINARY         = 0x08
      LIST           = 0x09
      SET            = 0x0A
      MAP            = 0x0B
      STRUCT         = 0x0C
      UUID           = 0x0D

      def self.is_bool_type?(b)
        (b & 0x0f) == BOOLEAN_TRUE || (b & 0x0f) == BOOLEAN_FALSE
      end

      COMPACT_TO_TTYPE = {
        Types::STOP   => Types::STOP,
        BOOLEAN_FALSE => Types::BOOL,
        BOOLEAN_TRUE  => Types::BOOL,
        BYTE          => Types::BYTE,
        I16           => Types::I16,
        I32           => Types::I32,
        I64           => Types::I64,
        DOUBLE        => Types::DOUBLE,
        BINARY        => Types::STRING,
        LIST          => Types::LIST,
        SET           => Types::SET,
        MAP           => Types::MAP,
        STRUCT        => Types::STRUCT,
        UUID          => Types::UUID
      }

      TTYPE_TO_COMPACT = {
        Types::STOP           => Types::STOP,
        Types::BOOL           => BOOLEAN_TRUE,
        Types::BYTE           => BYTE,
        Types::I16            => I16,
        Types::I32            => I32,
        Types::I64            => I64,
        Types::DOUBLE         => DOUBLE,
        Types::STRING         => BINARY,
        Types::LIST           => LIST,
        Types::SET            => SET,
        Types::MAP            => MAP,
        Types::STRUCT         => STRUCT,
        Types::UUID           => UUID
      }

      def self.get_ttype(compact_type)
        val = COMPACT_TO_TTYPE[compact_type & 0x0f]
        return val if val

        raise ProtocolException.new(
          ProtocolException::INVALID_DATA,
          "Unknown compact type: #{compact_type & 0x0f}"
        )
      end

      def self.get_compact_type(ttype)
        val = TTYPE_TO_COMPACT[ttype]
        return val if val

        raise ProtocolException.new(
          ProtocolException::INVALID_DATA,
          "Unknown compact type: #{ttype}"
        )
      end
    end

    def initialize(transport)
      super(transport)
      @reset_message_size = transport.message_boundaries?

      @last_field = [0]
      @boolean_value = nil

      # Pre-allocated read buffer for read_double().
      @rbuf = Bytes.empty_byte_buffer(8)
    end

    def write_message_begin(name, type, seqid)
      unless seqid.is_a?(Integer)
        raise 'nil argument not allowed!' if seqid.nil?
        raise ::TypeError, 'integer argument expected'
      end
      raise RangeError if seqid < I32_MIN || seqid > I32_MAX

      write_byte_direct(PROTOCOL_ID)
      write_byte_direct((VERSION & VERSION_MASK) | ((type << TYPE_SHIFT_AMOUNT) & TYPE_MASK))
      write_varint32(message_seqid_to_varint32(seqid))
      write_string(name)
      nil
    end

    def write_struct_begin(name)
      @last_field.push(0)
      nil
    end

    def write_struct_end
      @last_field.pop
      nil
    end

    def write_field_begin(name, type, id)
      unless id.is_a?(Integer)
        raise 'nil argument not allowed!' if id.nil?
        raise ::TypeError, 'integer argument expected'
      end
      raise RangeError if id < I16_MIN || id > I16_MAX

      if type == Types::BOOL
        # we want to possibly include the value, so we'll wait.
        @boolean_field = [type, id]
      else
        write_field_begin_internal(type, id)
      end
      nil
    end

    #
    # The workhorse of writeFieldBegin. It has the option of doing a
    # 'type override' of the type header. This is used specifically in the
    # boolean field case.
    #
    def write_field_begin_internal(type, id, type_override = nil)
      last_id = @last_field.pop

      # if there's a type override, use that.
      type_to_write = type_override || CompactTypes.get_compact_type(type)

      # check if we can use delta encoding for the field id
      if id > last_id && id - last_id <= 15
        # write them together
        write_byte_direct((id - last_id) << 4 | type_to_write)
      else
        # write them separate
        write_byte_direct(type_to_write)
        write_varint32(int_to_zig_zag(id))
      end

      @last_field.push(id)
      nil
    end

    def write_field_stop
      write_byte_direct(Types::STOP)
    end

    def write_map_begin(ktype, vtype, size)
      size = validate_size(size)
      if (size == 0)
        write_byte_direct(0)
      else
        write_varint32(size)
        write_byte_direct(CompactTypes.get_compact_type(ktype) << 4 | CompactTypes.get_compact_type(vtype))
      end
    end

    def write_list_begin(etype, size)
      write_collection_begin(etype, size)
    end

    def write_set_begin(etype, size)
      write_collection_begin(etype, size);
    end

    def write_bool(bool)
      # Preserve Ruby truthiness: only false and nil encode as false.
      type = bool ? CompactTypes::BOOLEAN_TRUE : CompactTypes::BOOLEAN_FALSE
      unless @boolean_field.nil?
        # we haven't written the field header yet
        write_field_begin_internal(@boolean_field.first, @boolean_field.last, type)
        @boolean_field = nil
      else
        # we're not part of a field, so just write the value.
        write_byte_direct(type)
      end
    end

    def write_byte(byte)
      unless byte.is_a?(Integer)
        raise 'nil argument not allowed!' if byte.nil?
        raise ::TypeError, 'integer argument expected'
      end
      raise RangeError if byte < BYTE_MIN || byte > BYTE_MAX

      write_byte_direct(byte)
    end

    def write_i16(i16)
      unless i16.is_a?(Integer)
        raise 'nil argument not allowed!' if i16.nil?
        raise ::TypeError, 'integer argument expected'
      end
      raise RangeError if i16 < I16_MIN || i16 > I16_MAX

      write_varint32(int_to_zig_zag(i16))
    end

    def write_i32(i32)
      unless i32.is_a?(Integer)
        raise 'nil argument not allowed!' if i32.nil?
        raise ::TypeError, 'integer argument expected'
      end
      raise RangeError if i32 < I32_MIN || i32 > I32_MAX

      write_varint32(int_to_zig_zag(i32))
    end

    def write_i64(i64)
      unless i64.is_a?(Integer)
        raise 'nil argument not allowed!' if i64.nil?
        raise ::TypeError, 'integer argument expected'
      end
      raise RangeError if i64 < I64_MIN || i64 > I64_MAX

      write_varint64(long_to_zig_zag(i64))
    end

    def write_double(dub)
      @trans.write([dub].pack("G").reverse)
    end

    def write_string(str)
      buf = Bytes.convert_to_utf8_byte_buffer(str)
      write_binary(buf)
    end

    def write_binary(buf)
      size = validate_size(buf.bytesize)
      write_varint32(size)
      @trans.write(buf)
    end

    def write_uuid(uuid)
      UUID.validate_uuid!(uuid)
      trans.write(UUID.uuid_bytes(uuid))
    end

    def read_message_begin
      trans.reset_message_size if @reset_message_size

      protocol_id = read_byte()
      if protocol_id != PROTOCOL_ID
        raise ProtocolException.new(
          ProtocolException::BAD_VERSION,
          "Expected protocol id #{PROTOCOL_ID} but got #{protocol_id}"
        )
      end

      version_and_type = read_byte()
      version = version_and_type & VERSION_MASK
      if (version != VERSION)
        raise ProtocolException.new(
          ProtocolException::BAD_VERSION,
          "Expected version #{VERSION} but got #{version}"
        )
      end

      type = (version_and_type >> TYPE_SHIFT_AMOUNT) & TYPE_BITS
      seqid = message_seqid_from_varint32(read_varint32())
      messageName = read_string()
      [messageName, type, seqid]
    end

    def read_struct_begin
      @last_field.push(0)
      ""
    end

    def read_struct_end
      @last_field.pop()
      nil
    end

    def read_field_begin
      type = read_byte()

      # if it's a stop, then we can return immediately, as the struct is over.
      if (type & 0x0f) == Types::STOP
        TSTOP
      else
        field_id = nil

        # mask off the 4 MSB of the type header. it could contain a field id delta.
        modifier = (type & 0xf0) >> 4
        if modifier == 0
          # not a delta. look ahead for the zigzag varint field id.
          @last_field.pop
          field_id = read_i16()
        else
          # has a delta. add the delta to the last read field id.
          field_id = @last_field.pop + modifier
        end

        # if this happens to be a boolean field, the value is encoded in the type
        if CompactTypes.is_bool_type?(type)
          # save the boolean value in a special instance variable.
          @bool_value = (type & 0x0f) == CompactTypes::BOOLEAN_TRUE
        end

        # push the new field onto the field stack so we can keep the deltas going.
        @last_field.push(field_id)
        [nil, CompactTypes.get_ttype(type & 0x0f), field_id]
      end
    end

    def read_map_begin
      size = read_varint32()
      validate_container_size(size)
      key_and_value_type = size == 0 ? 0 : read_byte()
      [CompactTypes.get_ttype(key_and_value_type >> 4), CompactTypes.get_ttype(key_and_value_type & 0xf), size]
    end

    def read_list_begin
      size_and_type = read_byte()
      size = (size_and_type >> 4) & 0x0f
      if size == 15
        size = read_varint32()
      end
      validate_container_size(size)
      type = CompactTypes.get_ttype(size_and_type)
      [type, size]
    end

    def read_set_begin
      read_list_begin
    end

    def read_bool
      unless @bool_value.nil?
        bv = @bool_value
        @bool_value = nil
        bv
      else
        read_byte() == CompactTypes::BOOLEAN_TRUE
      end
    end

    def read_byte
      val = trans.read_byte
      if (val > 0x7f)
        val = 0 - ((val - 1) ^ 0xff)
      end
      val
    end

    def read_i16
      zig_zag_to_int(read_varint32())
    end

    def read_i32
      zig_zag_to_int(read_varint32())
    end

    def read_i64
      zig_zag_to_long(read_varint64())
    end

    def read_double
      trans.read_into_buffer(@rbuf, 8)
      val = @rbuf.reverse.unpack('G').first
      val
    end

    def read_string
      buffer = read_binary
      Bytes.convert_to_string(buffer)
    end

    def read_binary
      size = read_varint32()
      if size > I32_MAX
        raise ProtocolException.new(ProtocolException::SIZE_LIMIT, 'Binary size limit exceeded')
      end
      trans.read_all(size)
    end

    def read_uuid
      UUID.uuid_from_bytes(trans.read_all(16))
    end

    def to_s
      "compact(#{super.to_s})"
    end

    private

    #
    # Abstract method for writing the start of lists and sets. List and sets on
    # the wire differ only by the type indicator.
    #
    def write_collection_begin(elem_type, size)
      size = validate_size(size)
      compact_type = CompactTypes.get_compact_type(elem_type)
      if size <= 14
        write_byte_direct(size << 4 | compact_type)
      else
        write_byte_direct(0xf0 | compact_type)
        write_varint32(size)
      end
    end

    def write_byte_direct(byte)
      @trans.write([byte].pack('C'))
    end

    def write_varint32(n)
      if (n & ~0x7F) == 0
        write_byte_direct(n)
        return
      end

      buffer = String.new(capacity: MAX_VARINT32_BYTES, encoding: Encoding::BINARY)
      while (n & ~0x7F) != 0
        buffer << ((n & 0x7F) | 0x80)
        n >>= 7
      end
      buffer << n
      @trans.write(buffer)
    end

    SEVEN_BIT_MASK = 0x7F
    EVERYTHING_ELSE_MASK = ~SEVEN_BIT_MASK

    def write_varint64(n)
      if (n & EVERYTHING_ELSE_MASK) == 0
        write_byte_direct(n)
        return
      end

      buffer = String.new(capacity: MAX_VARINT_BYTES, encoding: Encoding::BINARY)
      while (n & EVERYTHING_ELSE_MASK) != 0
        buffer << ((n & SEVEN_BIT_MASK) | 0x80)
        n >>= 7
      end
      buffer << n
      @trans.write(buffer)
    end

    def read_varint32()
      shift = 0
      result = 0
      VARINT32_PREFIX_BYTES.times do
        b = read_byte()
        result |= (b & 0x7f) << shift
        return result if (b & 0x80) != 0x80
        shift += 7
      end

      b = read_byte()
      if (b & 0x80) != 0
        raise ProtocolException.new(ProtocolException::INVALID_DATA, 'Variable-length int over 5 bytes.')
      end
      if (b & ~MAX_VARINT32_LAST_BYTE) != 0
        raise ProtocolException.new(ProtocolException::INVALID_DATA, 'Variable-length int overflows uint32.')
      end

      result | (b << shift)
    end

    def read_varint64()
      shift = 0
      result = 0
      MAX_VARINT_BYTES.times do
        b = read_byte()
        result |= (b & 0x7f) << shift
        return result if (b & 0x80) != 0x80
        shift += 7
      end
      raise ProtocolException.new(ProtocolException::INVALID_DATA, 'Variable-length int over 10 bytes.')
    end

    def int_to_zig_zag(n)
      (n << 1) ^ (n >> 31)
    end

    def long_to_zig_zag(l)
      # puts "zz encoded #{l} to #{(l << 1) ^ (l >> 63)}"
      (l << 1) ^ (l >> 63)
    end

    def zig_zag_to_int(n)
      (n >> 1) ^ -(n & 1)
    end

    def zig_zag_to_long(n)
      (n >> 1) ^ -(n & 1)
    end

    def message_seqid_to_varint32(seqid)
      seqid < 0 ? seqid + (2**32) : seqid
    end

    def validate_size(size)
      unless size.is_a?(Integer)
        raise 'nil argument not allowed!' if size.nil?
        raise ::TypeError, 'integer argument expected'
      end
      raise RangeError if size < 0 || size > I32_MAX

      size
    end

    def message_seqid_from_varint32(seqid)
      seqid > 0x7fffffff ? seqid - (2**32) : seqid
    end
  end

  class CompactProtocolFactory < BaseProtocolFactory
    def get_protocol(trans)
      CompactProtocol.new(trans)
    end

    def to_s
      "compact"
    end
  end
end
