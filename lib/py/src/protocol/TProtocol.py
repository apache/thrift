class TType:
  STOP   = 0
  VOID   = 1
  BOOL   = 2
  BYTE   = 3
  I08    = 4
  I16    = 6
  I32    = 8
  I64    = 10
  STRING = 11
  UTF7   = 11
  STRUCT = 12
  MAP    = 13
  SET    = 14
  LIST   = 15
  UTF8   = 16
  UTF16  = 17

class TMessageType:
  CALL  = 1
  REPLY = 2

class TProtocolBase:

  """Base class for Thrift protocol driver."""

  def writeMessageBegin(self, otrans, name, type, seqid):
    pass

  def writeMessageEnd(self, otrans):
    pass

  def writeStructBegin(self, otrans, name):
    pass

  def writeStructEnd(self, otrans):
    pass

  def writeFieldBegin(self, otrans, name, type, id):
    pass

  def writeFieldEnd(self, otrans):
    pass

  def writeFieldStop(self, otrans):
    pass

  def writeMapBegin(self, otrans, ktype, vtype, size):
    pass

  def writeMapEnd(self, otrans):
    pass

  def writeListBegin(self, otrans, etype, size):
    pass

  def writeListEnd(self, otrans):
    pass

  def writeSetBegin(self, otrans, etype, size):
    pass

  def writeSetEnd(self, otrans):
    pass

  def writeBool(self, otrans, bool):
    pass

  def writeByte(self, otrans, byte):
    pass

  def writeI16(self, otrans, i16):
    pass

  def writeI32(self, otrans, i32):
    pass

  def writeI64(self, otrans, i64):
    pass

  def writeString(self, otrans, str):
    pass

  def readMessageBegin(self, itrans):
    pass

  def readMessageEnd(self, itrans):
    pass

  def readStructBegin(self, itrans):
    pass

  def readStructEnd(self, itrans):
    pass

  def readFieldBegin(self, itrans):
    pass

  def readFieldEnd(self, itrans):
    pass

  def readMapBegin(self, itrans):
    pass

  def readMapEnd(self, itrans):
    pass

  def readListBegin(self, itrans):
    pass

  def readListEnd(self, itrans):
    pass

  def readSetBegin(self, itrans):
    pass

  def readSetEnd(self, itrans):
    pass

  def readBool(self, itrans):
    pass

  def readByte(self, itrans):
    pass

  def readI16(self, itrans):
    pass

  def readI32(self, itrans):
    pass

  def readI64(self, itrans):
    pass

  def readString(self, itrans):
    pass

  def skip(self, itrans, type):
    if type == TType.STOP:
      return
    elif type == TType.BOOL:
      self.readBool(itrans)
    elif type == TType.BYTE:
      self.readByte(itrans)
    elif type == TType.I16:
      self.readI16(itrans)
    elif type == TType.I32:
      self.readI32(itrans)
    elif type == TType.I64:
      self.readI64(itrans)
    elif type == TType.STRING:
      self.readString(itrans)
    elif type == TType.STRUCT:
      name = self.readStructBegin(itrans)
      while True:
        (name, type, id) = self.readFieldBegin(itrans)
        if type == TType.STOP:
          break
        self.skip(itrans, type)
        self.readFieldEnd(itrans)
      self.readStructEnd(itrans)
    elif type == TType.MAP:
      (ktype, vtype, size) = self.readMapBegin(itrans)
      for i in range(size):
        self.skip(itrans, ktype)
        self.skip(itrans, vtype)
      self.readMapEnd(itrans)
    elif type == TType.SET:
      (etype, size) = self.readSetBegin(itrans)
      for i in range(size):
        self.skip(itrans, etype)
      self.readSetEnd(itrans)
    elif type == TType.LIST:
      (etype, size) = self.readListBegin(itrans)
      for i in range(size):
        self.skip(itrans, etype)
      self.readListEnd(itrans)


    
