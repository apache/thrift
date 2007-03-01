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
class TType
  STOP = 0
  VOID = 1
  BOOL = 2
  BYTE = 3
  DOUBLE = 4
  I16 = 6
  I32 = 8
  I64 = 10
  STRING = 11
  STRUCT = 12
  MAP = 13
  SET = 14
  LIST = 15
end

class TMessageType
  CALL = 1
  REPLY = 2
end

class TProtocol
  
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
  
  def skip(type)
    if type === TType::STOP
      nil
    elsif type === TType::BOOL
      readBool()
    elsif type === TType::BYTE
      readByte()
    elsif type === TType::I16
      readI16()
    elsif type === TType::I32
      readI32()
    elsif type === TType::I64
      readI64()
    elsif type === TType::DOUBLE
      readDouble()
    elsif type === TType::STRING
      readString()
    elsif type === TType::STRUCT
      readStructBegin()
      while true
        name, type, id = readFieldBegin()
        if type === TType::STOP
          break
        else
          skip(type)
          readFieldEnd()
        end
        readStructEnd()
      end
    elsif type === TType::MAP
      ktype, vtype, size = readMapBegin()
      for i in 1..size
        skip(ktype)
        skip(vtype)
      end
      readMapEnd()
    elsif type === TType::SET
      etype, size = readSetBegin()
      for i in 1..size
        skip(etype)
      end
      readSetEnd()
    elsif type === TType::LIST
      etype, size = readListBegin()
      for i in 1..size
        skip(etype)
      end
      readListEnd()
    end
  end

end

class TProtocolFactory
  def getProtocol(trans); nil; end
end

