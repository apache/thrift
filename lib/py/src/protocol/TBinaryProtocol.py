from TProtocol import *
from struct import pack, unpack

class TBinaryProtocol(TProtocolBase):

  """Binary implementation of the Thrift protocol driver."""

  def __init__(self, itrans, otrans=None):
    TProtocolBase.__init__(self, itrans, otrans)

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
    self.otrans.write(buff)

  def writeI16(self, i16):
    buff = pack("!h", i16)
    self.otrans.write(buff)

  def writeI32(self, i32):
    buff = pack("!i", i32)
    self.otrans.write(buff)
    
  def writeI64(self, i64):
    buff = pack("!q", i64)
    self.otrans.write(buff)

  def writeDouble(self, dub):
    buff = pack("!d", dub)
    self.otrans.write(buff)

  def writeString(self, str):
    self.writeI32(len(str))
    self.otrans.write(str)

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
    buff = self.itrans.readAll(1)
    val, = unpack('!b', buff)
    return val

  def readI16(self):
    buff = self.itrans.readAll(2)
    val, = unpack('!h', buff)
    return val

  def readI32(self):
    buff = self.itrans.readAll(4)
    val, = unpack('!i', buff)
    return val

  def readI64(self):
    buff = self.itrans.readAll(8)
    val, = unpack('!q', buff)
    return val

  def readDouble(self):
    buff = self.itrans.readAll(8)
    val, = unpack('!d', buff)
    return val

  def readString(self):
    len = self.readI32()
    str = self.itrans.readAll(len)
    return str

class TBinaryProtocolFactory:
  def getIOProtocols(self, itrans, otrans):
    prot = TBinaryProtocol(itrans, otrans)
    return (prot, prot)
