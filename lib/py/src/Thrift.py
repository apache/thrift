class TProcessor:

  """Base class for procsessor, which works on two streams."""

  def process(iprot, oprot):
    pass

class TException(Exception):

  """Base class for all thrift exceptions."""

  def __init__(self, message=None):
    Exception.__init__(self, message)

class TApplicationException(TException):

  """Application level thrift exceptions."""

  UNKNOWN = 0
  UNKNOWN_METHOD = 1
  INVALID_MESSAGE_TYPE = 2
  WRONG_METHOD_NAME = 3
  BAD_SEQUENCE_ID = 4
  MISSING_RESULT = 5

  def __init__(self, type=UNKNOWN, message=None):
    TException.__init__(self, message)
    self.type = type

  def read(self, iprot):
    iprot.readStructBegin()
    while True:
      (fname, ftype, fid) = iprot.readFieldBegin()
      if ftype == TType.STOP:
        break
      if fid == 1:
        if ftype == TType.STRING:
          self.message = iprot.readString();
        else:
          iprot.skip(ftype)
      elif fid == 2:
        if ftype == TType.I32:
          self.type = iprot.readI32();
        else:
          iprot.skip(ftype)
      else:
        iprot.skip(ftype)
      iprot.readFieldEnd()
    iprot.readStructEnd()

  def write(self, oprot):
    oprot.writeStructBegin('TApplicationException')
    if self.message != None:
      oprot.writeFieldBegin('message', TType.STRING, 1)
      oprot.writeString(self.message)
      oprot.writeFieldEnd()
    if self.type != None:
      oprot.writeFieldBegin('type', TType.I32, 2)
      oprot.writeI32(self.type)
      oprot.writeFieldEnd()
    oprot.writeFieldStop()
    oprot.writeStructEnd()
