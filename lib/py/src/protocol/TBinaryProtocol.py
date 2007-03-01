#!/usr/bin/env python
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

from TProtocol import *
from struct import pack, unpack

class TBinaryProtocol(TProtocolBase):

  """Binary implementation of the Thrift protocol driver."""

  def __init__(self, trans):
    TProtocolBase.__init__(self, trans)

  def writeMessageBegin(self, name, type, seqid):
    self.writeString(name)
    self.writeByte(type)
    self.writeI32(seqid)

  def writeMessageEnd(self):
    pass

  def writeStructBegin(self, name):
    pass

  def writeStructEnd(self):
    pass

  def writeFieldBegin(self, name, type, id):
    self.writeByte(type)
    self.writeI16(id)

  def writeFieldEnd(self):
    pass

  def writeFieldStop(self):
    self.writeByte(TType.STOP);

  def writeMapBegin(self, ktype, vtype, size):
    self.writeByte(ktype)
    self.writeByte(vtype)
    self.writeI32(size)

  def writeMapEnd(self):
    pass

  def writeListBegin(self, etype, size):
    self.writeByte(etype)
    self.writeI32(size)

  def writeListEnd(self):
    pass

  def writeSetBegin(self, etype, size):
    self.writeByte(etype)
    self.writeI32(size)

  def writeSetEnd(self):
    pass

  def writeBool(self, bool):
    if bool:
      self.writeByte(1)
    else:
      self.writeByte(0)
    
  def writeByte(self, byte):
    buff = pack("!b", byte)
    self.trans.write(buff)

  def writeI16(self, i16):
    buff = pack("!h", i16)
    self.trans.write(buff)

  def writeI32(self, i32):
    buff = pack("!i", i32)
    self.trans.write(buff)
    
  def writeI64(self, i64):
    buff = pack("!q", i64)
    self.trans.write(buff)

  def writeDouble(self, dub):
    buff = pack("!d", dub)
    self.trans.write(buff)

  def writeString(self, str):
    self.writeI32(len(str))
    self.trans.write(str)

  def readMessageBegin(self):
    name = self.readString()
    type = self.readByte()
    seqid = self.readI32()
    return (name, type, seqid)

  def readMessageEnd(self):
    pass

  def readStructBegin(self):
    pass

  def readStructEnd(self):
    pass

  def readFieldBegin(self):
    type = self.readByte()
    if type == TType.STOP:
      return (None, type, 0)
    id = self.readI16()
    return (None, type, id)

  def readFieldEnd(self):
    pass

  def readMapBegin(self):
    ktype = self.readByte()
    vtype = self.readByte()
    size = self.readI32()
    return (ktype, vtype, size)

  def readMapEnd(self):
    pass

  def readListBegin(self):
    etype = self.readByte()
    size = self.readI32()
    return (etype, size)

  def readListEnd(self):
    pass

  def readSetBegin(self):
    etype = self.readByte()
    size = self.readI32()
    return (etype, size)

  def readSetEnd(self):
    pass

  def readBool(self):
    byte = self.readByte()
    if byte == 0:
      return False
    return True

  def readByte(self):
    buff = self.trans.readAll(1)
    val, = unpack('!b', buff)
    return val

  def readI16(self):
    buff = self.trans.readAll(2)
    val, = unpack('!h', buff)
    return val

  def readI32(self):
    buff = self.trans.readAll(4)
    val, = unpack('!i', buff)
    return val

  def readI64(self):
    buff = self.trans.readAll(8)
    val, = unpack('!q', buff)
    return val

  def readDouble(self):
    buff = self.trans.readAll(8)
    val, = unpack('!d', buff)
    return val

  def readString(self):
    len = self.readI32()
    str = self.trans.readAll(len)
    return str

class TBinaryProtocolFactory:
  def getProtocol(self, trans):
    prot = TBinaryProtocol(trans)
    return prot
