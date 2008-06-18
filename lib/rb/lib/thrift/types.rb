module Thrift
  module Types
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
  deprecate_module! :TType => Types

  module MessageTypes
    CALL = 1
    REPLY = 2
    EXCEPTION = 3
  end
  deprecate_module! :TMessageType => MessageTypes
end
