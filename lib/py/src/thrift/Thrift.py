#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Callable, NoReturn

if TYPE_CHECKING:
    from thrift.protocol.TProtocol import TProtocolBase


class TType:
    """Thrift type constants."""

    STOP: int = 0
    VOID: int = 1
    BOOL: int = 2
    BYTE: int = 3
    I08: int = 3
    DOUBLE: int = 4
    I16: int = 6
    I32: int = 8
    I64: int = 10
    STRING: int = 11
    UTF7: int = 11
    STRUCT: int = 12
    MAP: int = 13
    SET: int = 14
    LIST: int = 15
    UTF8: int = 16
    UTF16: int = 17

    _VALUES_TO_NAMES: tuple[str | None, ...] = (
        'STOP',
        'VOID',
        'BOOL',
        'BYTE',
        'DOUBLE',
        None,
        'I16',
        None,
        'I32',
        None,
        'I64',
        'STRING',
        'STRUCT',
        'MAP',
        'SET',
        'LIST',
        'UTF8',
        'UTF16',
    )


class TMessageType:
    """Thrift message type constants."""

    CALL: int = 1
    REPLY: int = 2
    EXCEPTION: int = 3
    ONEWAY: int = 4


class TProcessor:
    """Base class for processor, which works on two streams."""

    def process(self, iprot: TProtocolBase, oprot: TProtocolBase) -> bool | None:
        """
        Process a request.  The normal behvaior is to have the
        processor invoke the correct handler and then it is the
        server's responsibility to write the response to oprot.
        """
        pass

    def on_message_begin(self, func: Callable[[str, int, int], None]) -> None:
        """
        Install a callback that receives (name, type, seqid)
        after the message header is read.
        """
        pass


class TException(Exception):
    """Base class for all thrift exceptions."""

    message: str | None

    def __init__(self, message: str | None = None) -> None:
        Exception.__init__(self, message)
        super(TException, self).__setattr__("message", message)


class TApplicationException(TException):
    """Application level thrift exceptions."""

    UNKNOWN: int = 0
    UNKNOWN_METHOD: int = 1
    INVALID_MESSAGE_TYPE: int = 2
    WRONG_METHOD_NAME: int = 3
    BAD_SEQUENCE_ID: int = 4
    MISSING_RESULT: int = 5
    INTERNAL_ERROR: int = 6
    PROTOCOL_ERROR: int = 7
    INVALID_TRANSFORM: int = 8
    INVALID_PROTOCOL: int = 9
    UNSUPPORTED_CLIENT_TYPE: int = 10

    type: int

    def __init__(self, type: int = UNKNOWN, message: str | None = None) -> None:
        TException.__init__(self, message)
        self.type = type

    def __str__(self) -> str:
        if self.message:
            return self.message
        elif self.type == self.UNKNOWN_METHOD:
            return 'Unknown method'
        elif self.type == self.INVALID_MESSAGE_TYPE:
            return 'Invalid message type'
        elif self.type == self.WRONG_METHOD_NAME:
            return 'Wrong method name'
        elif self.type == self.BAD_SEQUENCE_ID:
            return 'Bad sequence ID'
        elif self.type == self.MISSING_RESULT:
            return 'Missing result'
        elif self.type == self.INTERNAL_ERROR:
            return 'Internal error'
        elif self.type == self.PROTOCOL_ERROR:
            return 'Protocol error'
        elif self.type == self.INVALID_TRANSFORM:
            return 'Invalid transform'
        elif self.type == self.INVALID_PROTOCOL:
            return 'Invalid protocol'
        elif self.type == self.UNSUPPORTED_CLIENT_TYPE:
            return 'Unsupported client type'
        else:
            return 'Default (unknown) TApplicationException'

    def read(self, iprot: TProtocolBase) -> None:
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.message = iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I32:
                    self.type = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot: TProtocolBase) -> None:
        oprot.writeStructBegin('TApplicationException')
        if self.message is not None:
            oprot.writeFieldBegin('message', TType.STRING, 1)
            oprot.writeString(self.message)
            oprot.writeFieldEnd()
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.I32, 2)
            oprot.writeI32(self.type)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()


class TFrozenDict(dict[Any, Any]):
    """A dictionary that is "frozen" like a frozenset"""

    __hashval: int

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super(TFrozenDict, self).__init__(*args, **kwargs)
        # Sort the items so they will be in a consistent order.
        # XOR in the hash of the class so we don't collide with
        # the hash of a list of tuples.
        self.__hashval = hash(TFrozenDict) ^ hash(tuple(sorted(self.items())))

    def __setitem__(self, *args: Any) -> NoReturn:
        raise TypeError("Can't modify frozen TFreezableDict")

    def __delitem__(self, *args: Any) -> NoReturn:
        raise TypeError("Can't modify frozen TFreezableDict")

    def __hash__(self) -> int:  # type: ignore[override]
        return self.__hashval
