#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#
require 'thrift/protocol'

module Thrift
  class BinaryProtocol < Protocol
    VERSION_MASK = 0xffff0000
    VERSION_1 = 0x80010000

    def write_message_begin(name, type, seqid)
      write_i32(VERSION_1 | type)
      write_string(name)
      write_i32(seqid)
    end

    def write_field_begin(name, type, id)
      write_byte(type)
      write_i16(id)
    end

    def write_field_stop
      write_byte(Thrift::Types::STOP)
    end

    def write_map_begin(ktype, vtype, size)
      write_byte(ktype)
      write_byte(vtype)
      write_i32(size)
    end

    def write_list_begin(etype, size)
      write_byte(etype)
      write_i32(size)
    end

    def write_set_begin(etype, size)
      write_byte(etype)
      write_i32(size)
    end

    def write_bool(bool)
      if (bool)
        write_byte(1)
      else
        write_byte(0)
      end
    end

    def write_byte(byte)
      # yes, -128..255. This covers signed byte min -> unsigned byte max
      raise RangeError.new("#{byte} too large to fit in a byte") unless (-128..127).include? byte
      trans.write([byte].pack('n')[1..1])
    end

    def write_i16(i16)
      raise RangeError.new("#{i16} too large to fit in an i16") unless ((-2**15)..(2**15-1)).include? i16
      trans.write([i16].pack('n'))
    end

    def write_i32(i32)
      raise RangeError.new("#{i32} too large to fit in an i32") unless ((-2**31)..(2**31-1)).include? i32
      trans.write([i32].pack('N'))
    end

    def write_i64(i64)
      raise RangeError.new("#{i64} too large to fit in an i32") unless ((-2**63)..(2**63-1)).include? i64
      hi = i64 >> 32
      lo = i64 & 0xffffffff
      trans.write([hi, lo].pack('N2'))
    end

    def write_double(dub)
      trans.write([dub].pack('G'))
    end

    def write_string(str)
      write_i32(str.length)
      trans.write(str)
    end

    def read_message_begin
      version = read_i32
      if (version & VERSION_MASK != VERSION_1)
        raise ProtocolException.new(ProtocolException::BAD_VERSION, 'Missing version identifier')
      end
      type = version & 0x000000ff
      name = read_string
      seqid = read_i32
      return name, type, seqid
    end

    def read_field_begin
      type = read_byte
      if (type === Types::STOP)
        return nil, type, 0
      end
      id = read_i16
      return nil, type, id
    end

    def read_map_begin
      ktype = read_byte
      vtype = read_byte
      size = read_i32
      return ktype, vtype, size
    end

    def read_list_begin
      etype = read_byte
      size = read_i32
      return etype, size
    end

    def read_set_begin
      etype = read_byte
      size = read_i32
      return etype, size
    end

    def read_bool
      byte = read_byte
      return byte != 0
    end

    def read_byte
      dat = trans.read_all(1)
      val = dat[0]
      if (val > 0x7f)
        val = 0 - ((val - 1) ^ 0xff)
      end
      return val
    end

    def read_i16
      dat = trans.read_all(2)
      val, = dat.unpack('n')
      if (val > 0x7fff)
        val = 0 - ((val - 1) ^ 0xffff)
      end
      return val
    end

    def read_i32
      dat = trans.read_all(4)
      val, = dat.unpack('N')
      if (val > 0x7fffffff)
        val = 0 - ((val - 1) ^ 0xffffffff)
      end
      return val
    end

    def read_i64
      dat = trans.read_all(8)
      hi, lo = dat.unpack('N2')
      if (hi > 0x7fffffff)
        hi = hi ^ 0xffffffff
        lo = lo ^ 0xffffffff
        return 0 - hi*4294967296 - lo - 1
      else
        return hi*4294967296 + lo
      end
    end

    def read_double
      dat = trans.read_all(8)
      val, = dat.unpack('G')
      return val
    end

    def read_string
      sz = read_i32
      dat = trans.read_all(sz)
      return dat
    end

  end
  deprecate_class! :TBinaryProtocol => BinaryProtocol

  class BinaryProtocolFactory < ProtocolFactory
    def get_protocol(trans)
      return Thrift::BinaryProtocol.new(trans)
    end
  end
  deprecate_class! :TBinaryProtocolFactory => BinaryProtocolFactory
end
