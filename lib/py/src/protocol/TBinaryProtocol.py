from TProtocol import *
from struct import pack, unpack

class TBinaryProtocol(TProtocolBase):

  """Binary implementation of the Thrift protocol driver."""

  def writeMessageBegin(self, otrans, name, type, seqid):
    self.writeString(otrans, name)
    self.writeByte(otrans, type)
    self.writeI32(otrans, seqid)

  def writeMessageEnd(self, otrans):
    pass

  def writeStructBegin(self, otrans, name):
    pass

  def writeStructEnd(self, otrans):
    pass

  def writeFieldBegin(self, otrans, name, type, id):
    self.writeByte(otrans, type)
    self.writeI16(otrans, id)

  def writeFieldEnd(self, otrans):
    pass

  def writeFieldStop(self, otrans):
    self.writeByte(otrans, TType.STOP);

  def writeMapBegin(self, otrans, ktype, vtype, size):
    self.writeByte(otrans, ktype)
    self.writeByte(otrans, vtype)
    self.writeI32(otrans, size)

  def writeMapEnd(self, otrans):
    pass

  def writeListBegin(self, otrans, etype, size):
    self.writeByte(otrans, etype)
    self.writeI32(otrans, size)

  def writeListEnd(self, otrans):
    pass

  def writeSetBegin(self, otrans, etype, size):
    self.writeByte(otrans, etype)
    self.writeI32(otrans, size)

  def writeSetEnd(self, otrans):
    pass

  def writeBool(self, otrans, bool):
    if bool:
      self.writeByte(otrans, 1)
    else:
      self.writeByte(otrans, 0)
    
  def writeByte(self, otrans, byte):
    buff = pack("!b", byte)
    otrans.write(buff)

  def writeI16(self, otrans, i16):
    buff = pack("!h", i16)
    otrans.write(buff)

  def writeI32(self, otrans, i32):
    buff = pack("!i", i32)
    otrans.write(buff)
    
  def writeI64(self, otrans, i64):
    buff = pack("!l", i64)
    otrans.write(buff)

  def writeString(self, otrans, str):
    self.writeI32(otrans, len(str))
    otrans.write(str)

  def readMessageBegin(self, itrans):
    name = self.readString(itrans)
    type = self.readByte(itrans)
    seqid = self.readI32(itrans)
    return (name, type, seqid)

  def readMessageEnd(self, itrans):
    pass

  def readStructBegin(self, itrans):
    pass

  def readStructEnd(self, itrans):
    pass

  def readFieldBegin(self, itrans):
    type = self.readByte(itrans)
    if type == TType.STOP:
      return (None, type, 0)
    id = self.readI16(itrans)
    return (None, type, id)

  def readFieldEnd(self, itrans):
    pass

  def readMapBegin(self, itrans):
    ktype = self.readByte(itrans)
    vtype = self.readByte(itrans)
    size = self.readI32(itrans)
    return (ktype, vtype, size)

  def readMapEnd(self, itrans):
    pass

  def readListBegin(self, itrans):
    etype = self.readByte(itrans)
    size = self.readI32(itrans)
    return (etype, size)

  def readListEnd(self, itrans):
    pass

  def readSetBegin(self, itrans):
    etype = self.readByte(itrans)
    size = self.readI32(itrans)
    return (etype, size)

  def readSetEnd(self, itrans):
    pass

  def readBool(self, itrans):
    byte = self.readByte(itrans)
    if byte == 0:
      return False
    return True

  def readByte(self, itrans):
    buff = itrans.readAll(1)
    val, = unpack('!b', buff)
    return val

  def readI16(self, itrans):
    buff = itrans.readAll(2)
    val, = unpack('!h', buff)
    return val

  def readI32(self, itrans):
    buff = itrans.readAll(4)
    val, = unpack('!i', buff)
    return val

  def readI64(self, itrans):
    buff = itrans.readAll(8)
    val, = unpack('!l', buff)
    return val

  def readString(self, itrans):
    len = self.readI32(itrans)
    str = itrans.readAll(len)
    return str
