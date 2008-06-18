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

module Thrift
  class ProtocolException < Exception

    UNKNOWN = 0
    INVALID_DATA = 1
    NEGATIVE_SIZE = 2
    SIZE_LIMIT = 3
    BAD_VERSION = 4

    attr_reader :type

    def initialize(type=UNKNOWN, message=nil)
      super(message)
      @type = type
    end

  end

  class Protocol

    attr_reader :trans

    def initialize(trans)
      @trans = trans
    end

    def writeMessageBegin(name, type, seqid); nil; end

    def writeMessageEnd; nil; end

    def writeStructBegin(name); nil; end

    def writeStructEnd(); nil; end

    def writeFieldBegin(name, type, id); nil; end

    def writeFieldEnd(); nil; end

    def writeFieldStop(); nil; end

    def writeMapBegin(ktype, vtype, size); nil; end

    def writeMapEnd(); nil; end

    def writeListBegin(etype, size); nil; end

    def writeListEnd(); nil; end

    def writeSetBegin(etype, size); nil; end

    def writeSetEnd(); nil; end

    def writeBool(bool); nil; end

    def writeByte(byte); nil; end

    def writeI16(i16); nil; end

    def writeI32(i32); nil; end

    def writeI64(i64); nil; end

    def writeDouble(dub); nil; end

    def writeString(str); nil; end

    def readMessageBegin(); nil; end

    def readMessageEnd(); nil; end

    def readStructBegin(); nil; end

    def readStructEnd(); nil; end

    def readFieldBegin(); nil; end

    def readFieldEnd(); nil; end

    def readMapBegin(); nil; end

    def readMapEnd(); nil; end

    def readListBegin(); nil; end

    def readListEnd(); nil; end

    def readSetBegin(); nil; end

    def readSetEnd(); nil; end

    def readBool(); nil; end

    def readByte(); nil; end

    def readI16(); nil; end

    def readI32(); nil; end

    def readI64(); nil; end

    def readDouble(); nil; end

    def readString(); nil; end

    def write_field(name, type, fid, value)
      writeFieldBegin(name, type, fid)
      write_type(type, value)
      writeFieldEnd
    end

    def write_type(type, value)
      case type
      when Types::BOOL
        writeBool(value)
      when Types::BYTE
        writeByte(value)
      when Types::DOUBLE
        writeDouble(value)
      when Types::I16
        writeI16(value)
      when Types::I32
        writeI32(value)
      when Types::I64
        writeI64(value)
      when Types::STRING
        writeString(value)
      when Types::STRUCT
        value.write(self)
      else
        raise NotImplementedError
      end
    end

    def read_type(type)
      case type
      when Types::BOOL
        readBool
      when Types::BYTE
        readByte
      when Types::DOUBLE
        readDouble
      when Types::I16
        readI16
      when Types::I32
        readI32
      when Types::I64
        readI64
      when Types::STRING
        readString
      else
        raise NotImplementedError
      end
    end

    def skip(type)
      if type === Types::STOP
        nil
      elsif type === Types::BOOL
        readBool()
      elsif type === Types::BYTE
        readByte()
      elsif type === Types::I16
        readI16()
      elsif type === Types::I32
        readI32()
      elsif type === Types::I64
        readI64()
      elsif type === Types::DOUBLE
        readDouble()
      elsif type === Types::STRING
        readString()
      elsif type === Types::STRUCT
        readStructBegin()
        while true
          name, type, id = readFieldBegin()
          if type === Types::STOP
            break
          else
            skip(type)
            readFieldEnd()
          end
          readStructEnd()
        end
      elsif type === Types::MAP
        ktype, vtype, size = readMapBegin()
        for i in 1..size
          skip(ktype)
          skip(vtype)
        end
        readMapEnd()
      elsif type === Types::SET
        etype, size = readSetBegin()
        for i in 1..size
          skip(etype)
        end
        readSetEnd()
      elsif type === Types::LIST
        etype, size = readListBegin()
        for i in 1..size
          skip(etype)
        end
        readListEnd()
      end
    end

  end
end

class TProtocolFactory
  def getProtocol(trans); nil; end
end
