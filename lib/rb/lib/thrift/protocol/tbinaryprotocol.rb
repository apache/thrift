#!/usr/bin/env ruby
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#
require 'thrift/protocol/tprotocol'

module Thrift
  class BinaryProtocol < Protocol
    VERSION_MASK = 0xffff0000
    VERSION_1 = 0x80010000

    def initialize(trans)
      super(trans)
    end

    def writeMessageBegin(name, type, seqid)
      writeI32(VERSION_1 | type)
      writeString(name)
      writeI32(seqid)
    end

    def writeFieldBegin(name, type, id)
      writeByte(type)
      writeI16(id)
    end

    def writeFieldStop()
      writeByte(Thrift::Types::STOP)
    end

    def writeMapBegin(ktype, vtype, size)
      writeByte(ktype)
      writeByte(vtype)
      writeI32(size)
    end

    def writeListBegin(etype, size)
      writeByte(etype)
      writeI32(size)
    end

    def writeSetBegin(etype, size)
      writeByte(etype)
      writeI32(size)
    end

    def writeBool(bool)
      if (bool)
        writeByte(1)
      else
        writeByte(0)
      end
    end

    def writeByte(byte)
      trans.write([byte].pack('n')[1..1])
    end

    def writeI16(i16)
      trans.write([i16].pack('n'))
    end

    def writeI32(i32)
      trans.write([i32].pack('N'))
    end

    def writeI64(i64)
      hi = i64 >> 32
      lo = i64 & 0xffffffff
      trans.write([hi, lo].pack('N2'))
    end

    def writeDouble(dub)
      trans.write([dub].pack('G'))
    end

    def writeString(str)
      writeI32(str.length)
      trans.write(str)
    end

    def readMessageBegin()
      version = readI32()
      if (version & VERSION_MASK != VERSION_1)
        raise ProtocolException.new(ProtocolException::BAD_VERSION, 'Missing version identifier')
      end
      type = version & 0x000000ff
      name = readString()
      seqid = readI32()
      return name, type, seqid
    end

    def readFieldBegin()
      type = readByte()
      if (type === Types::STOP)
        return nil, type, 0
      end
      id = readI16()
      return nil, type, id
    end

    def readMapBegin()
      ktype = readByte()
      vtype = readByte()
      size = readI32()
      return ktype, vtype, size
    end

    def readListBegin()
      etype = readByte()
      size = readI32()
      return etype, size
    end

    def readSetBegin()
      etype = readByte()
      size = readI32()
      return etype, size
    end

    def readBool()
      byte = readByte()
      return byte != 0
    end

    def readByte()
      dat = trans.readAll(1)
      val = dat[0]
      if (val > 0x7f)
        val = 0 - ((val - 1) ^ 0xff)
      end
      return val
    end

    def readI16()
      dat = trans.readAll(2)
      val, = dat.unpack('n')
      if (val > 0x7fff)
        val = 0 - ((val - 1) ^ 0xffff)
      end
      return val
    end

    def readI32()
      dat = trans.readAll(4)
      val, = dat.unpack('N')
      if (val > 0x7fffffff)
        val = 0 - ((val - 1) ^ 0xffffffff)
      end
      return val
    end

    def readI64()
      dat = trans.readAll(8)
      hi, lo = dat.unpack('N2')
      if (hi > 0x7fffffff)
        hi = hi ^ 0xffffffff
        lo = lo ^ 0xffffffff
        return 0 - hi*4294967296 - lo - 1
      else
        return hi*4294967296 + lo
      end
    end

    def readDouble()
      dat = trans.readAll(8)
      val, = dat.unpack('G')
      return val
    end

    def readString()
      sz = readI32()
      dat = trans.readAll(sz)
      return dat
    end

  end
end

TBinaryProtocol = Thrift::BinaryProtocol

class TBinaryProtocolFactory < TProtocolFactory
  def getProtocol(trans)
    return TBinaryProtocol.new(trans)
  end
end

