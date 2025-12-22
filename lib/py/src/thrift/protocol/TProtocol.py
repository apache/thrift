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

import sys
from abc import ABC, abstractmethod
from itertools import islice
from typing import Any, Generator, Iterable

from thrift.Thrift import TException, TFrozenDict, TType
from thrift.transport.TTransport import TTransportBase, TTransportException


class TProtocolException(TException):
    """Custom Protocol Exception class"""

    UNKNOWN: int = 0
    INVALID_DATA: int = 1
    NEGATIVE_SIZE: int = 2
    SIZE_LIMIT: int = 3
    BAD_VERSION: int = 4
    NOT_IMPLEMENTED: int = 5
    DEPTH_LIMIT: int = 6
    INVALID_PROTOCOL: int = 7

    type: int

    def __init__(self, type: int = UNKNOWN, message: str | None = None) -> None:
        TException.__init__(self, message)
        self.type = type


class TProtocolBase(ABC):
    """Base class for Thrift protocol driver."""

    trans: TTransportBase
    _fast_decode: Any
    _fast_encode: Any

    def __init__(self, trans: TTransportBase) -> None:
        self.trans = trans
        self._fast_decode = None
        self._fast_encode = None

    @staticmethod
    def _check_length(limit: int | None, length: int) -> None:
        if length < 0:
            raise TTransportException(
                TTransportException.NEGATIVE_SIZE, 'Negative length: %d' % length
            )
        if limit is not None and length > limit:
            raise TTransportException(
                TTransportException.SIZE_LIMIT, 'Length exceeded max allowed: %d' % limit
            )

    def writeMessageBegin(self, name: str, ttype: int, seqid: int) -> None:
        pass

    def writeMessageEnd(self) -> None:
        pass

    def writeStructBegin(self, name: str) -> None:
        pass

    def writeStructEnd(self) -> None:
        pass

    def writeFieldBegin(self, name: str, ttype: int, fid: int) -> None:
        pass

    def writeFieldEnd(self) -> None:
        pass

    def writeFieldStop(self) -> None:
        pass

    def writeMapBegin(self, ktype: int, vtype: int, size: int) -> None:
        pass

    def writeMapEnd(self) -> None:
        pass

    def writeListBegin(self, etype: int, size: int) -> None:
        pass

    def writeListEnd(self) -> None:
        pass

    def writeSetBegin(self, etype: int, size: int) -> None:
        pass

    def writeSetEnd(self) -> None:
        pass

    def writeBool(self, bool_val: bool) -> None:
        pass

    def writeByte(self, byte: int) -> None:
        pass

    def writeI16(self, i16: int) -> None:
        pass

    def writeI32(self, i32: int) -> None:
        pass

    def writeI64(self, i64: int) -> None:
        pass

    def writeDouble(self, dub: float) -> None:
        pass

    def writeString(self, str_val: str) -> None:
        self.writeBinary(bytes(str_val, 'utf-8'))

    def writeBinary(self, str_val: bytes) -> None:
        pass

    @abstractmethod
    def readMessageBegin(self) -> tuple[str, int, int]:
        pass

    def readMessageEnd(self) -> None:
        pass

    def readStructBegin(self) -> str | None:
        pass

    def readStructEnd(self) -> None:
        pass

    @abstractmethod
    def readFieldBegin(self) -> tuple[str | None, int, int]:
        pass

    def readFieldEnd(self) -> None:
        pass

    @abstractmethod
    def readMapBegin(self) -> tuple[int, int, int]:
        pass

    def readMapEnd(self) -> None:
        pass

    @abstractmethod
    def readListBegin(self) -> tuple[int, int]:
        pass

    def readListEnd(self) -> None:
        pass

    @abstractmethod
    def readSetBegin(self) -> tuple[int, int]:
        pass

    def readSetEnd(self) -> None:
        pass

    @abstractmethod
    def readBool(self) -> bool:
        pass

    @abstractmethod
    def readByte(self) -> int:
        pass

    @abstractmethod
    def readI16(self) -> int:
        pass

    @abstractmethod
    def readI32(self) -> int:
        pass

    @abstractmethod
    def readI64(self) -> int:
        pass

    @abstractmethod
    def readDouble(self) -> float:
        pass

    def readString(self) -> str:
        return self.readBinary().decode('utf-8')

    @abstractmethod
    def readBinary(self) -> bytes:
        pass

    def skip(self, ttype: int) -> None:
        if ttype == TType.BOOL:
            self.readBool()
        elif ttype == TType.BYTE:
            self.readByte()
        elif ttype == TType.I16:
            self.readI16()
        elif ttype == TType.I32:
            self.readI32()
        elif ttype == TType.I64:
            self.readI64()
        elif ttype == TType.DOUBLE:
            self.readDouble()
        elif ttype == TType.STRING:
            self.readString()
        elif ttype == TType.STRUCT:
            name = self.readStructBegin()
            while True:
                (name, ttype, id) = self.readFieldBegin()
                if ttype == TType.STOP:
                    break
                self.skip(ttype)
                self.readFieldEnd()
            self.readStructEnd()
        elif ttype == TType.MAP:
            (ktype, vtype, size) = self.readMapBegin()
            for i in range(size):
                self.skip(ktype)
                self.skip(vtype)
            self.readMapEnd()
        elif ttype == TType.SET:
            (etype, size) = self.readSetBegin()
            for i in range(size):
                self.skip(etype)
            self.readSetEnd()
        elif ttype == TType.LIST:
            (etype, size) = self.readListBegin()
            for i in range(size):
                self.skip(etype)
            self.readListEnd()
        else:
            raise TProtocolException(TProtocolException.INVALID_DATA, "invalid TType")

    # tuple of: ( 'reader method' name, is_container bool, 'writer_method' name )
    _TTYPE_HANDLERS: tuple[
        tuple[str | None, str | None, bool], ...
    ] = (
        (None, None, False),  # 0 TType.STOP
        (None, None, False),  # 1 TType.VOID # TODO: handle void?
        ('readBool', 'writeBool', False),  # 2 TType.BOOL
        ('readByte', 'writeByte', False),  # 3 TType.BYTE and I08
        ('readDouble', 'writeDouble', False),  # 4 TType.DOUBLE
        (None, None, False),  # 5 undefined
        ('readI16', 'writeI16', False),  # 6 TType.I16
        (None, None, False),  # 7 undefined
        ('readI32', 'writeI32', False),  # 8 TType.I32
        (None, None, False),  # 9 undefined
        ('readI64', 'writeI64', False),  # 10 TType.I64
        ('readString', 'writeString', False),  # 11 TType.STRING and UTF7
        ('readContainerStruct', 'writeContainerStruct', True),  # 12 *.STRUCT
        ('readContainerMap', 'writeContainerMap', True),  # 13 TType.MAP
        ('readContainerSet', 'writeContainerSet', True),  # 14 TType.SET
        ('readContainerList', 'writeContainerList', True),  # 15 TType.LIST
        (None, None, False),  # 16 TType.UTF8 # TODO: handle utf8 types?
        (None, None, False),  # 17 TType.UTF16 # TODO: handle utf16 types?
    )

    def _ttype_handlers(
        self, ttype: int, spec: Any
    ) -> tuple[str | None, str | None, bool]:
        if spec == 'BINARY':
            if ttype != TType.STRING:
                raise TProtocolException(
                    type=TProtocolException.INVALID_DATA,
                    message='Invalid binary field type %d' % ttype,
                )
            return ('readBinary', 'writeBinary', False)
        return (
            self._TTYPE_HANDLERS[ttype]
            if ttype < len(self._TTYPE_HANDLERS)
            else (None, None, False)
        )

    def _read_by_ttype(
        self, ttype: int, spec: Any, espec: Any
    ) -> Generator[Any, None, None]:
        reader_name, _, is_container = self._ttype_handlers(ttype, espec)
        if reader_name is None:
            raise TProtocolException(
                type=TProtocolException.INVALID_DATA, message='Invalid type %d' % (ttype)
            )
        reader_func = getattr(self, reader_name)
        read = (lambda: reader_func(espec)) if is_container else reader_func
        while True:
            yield read()

    def readFieldByTType(self, ttype: int, spec: Any) -> Any:
        return next(self._read_by_ttype(ttype, spec, spec))

    def readContainerList(self, spec: tuple[int, Any, bool]) -> list[Any] | tuple[Any, ...]:
        ttype, tspec, is_immutable = spec
        (list_type, list_len) = self.readListBegin()
        # TODO: compare types we just decoded with thrift_spec
        elems = islice(self._read_by_ttype(ttype, spec, tspec), list_len)
        results: list[Any] | tuple[Any, ...] = (tuple if is_immutable else list)(elems)
        self.readListEnd()
        return results

    def readContainerSet(self, spec: tuple[int, Any, bool]) -> set[Any] | frozenset[Any]:
        ttype, tspec, is_immutable = spec
        (set_type, set_len) = self.readSetBegin()
        # TODO: compare types we just decoded with thrift_spec
        elems = islice(self._read_by_ttype(ttype, spec, tspec), set_len)
        results: set[Any] | frozenset[Any] = (frozenset if is_immutable else set)(elems)
        self.readSetEnd()
        return results

    def readContainerStruct(self, spec: tuple[type, Any]) -> Any:
        (obj_class, obj_spec) = spec

        # If obj_class.read is a classmethod (e.g. in frozen structs),
        # call it as such.
        read_method = getattr(obj_class, 'read', None)
        if read_method is not None and getattr(read_method, '__self__', None) is obj_class:
            obj = read_method(self)
        else:
            obj = obj_class()
            obj.read(self)
        return obj

    def readContainerMap(
        self, spec: tuple[int, Any, int, Any, bool]
    ) -> dict[Any, Any] | TFrozenDict:
        ktype, kspec, vtype, vspec, is_immutable = spec
        (map_ktype, map_vtype, map_len) = self.readMapBegin()
        # TODO: compare types we just decoded with thrift_spec and
        # abort/skip if types disagree
        keys = self._read_by_ttype(ktype, spec, kspec)
        vals = self._read_by_ttype(vtype, spec, vspec)
        keyvals = islice(zip(keys, vals), map_len)
        results: dict[Any, Any] | TFrozenDict = (TFrozenDict if is_immutable else dict)(
            keyvals
        )
        self.readMapEnd()
        return results

    def readStruct(
        self, obj: Any, thrift_spec: tuple[Any, ...], is_immutable: bool = False
    ) -> Any:
        fields: dict[str, Any] = {} if is_immutable else {}
        self.readStructBegin()
        while True:
            (fname, ftype, fid) = self.readFieldBegin()
            if ftype == TType.STOP:
                break
            try:
                field = thrift_spec[fid]
            except IndexError:
                self.skip(ftype)
            else:
                if field is not None and ftype == field[1]:
                    fname = field[2]
                    fspec = field[3]
                    val = self.readFieldByTType(ftype, fspec)
                    if is_immutable:
                        fields[fname] = val
                    else:
                        setattr(obj, fname, val)
                else:
                    self.skip(ftype)
            self.readFieldEnd()
        self.readStructEnd()
        if is_immutable:
            return obj(**fields)
        return None

    def writeContainerStruct(self, val: Any, spec: Any) -> None:
        val.write(self)

    def writeContainerList(self, val: list[Any] | tuple[Any, ...], spec: tuple[int, Any, bool]) -> None:
        ttype, tspec, _ = spec
        self.writeListBegin(ttype, len(val))
        for _ in self._write_by_ttype(ttype, val, spec, tspec):
            pass
        self.writeListEnd()

    def writeContainerSet(self, val: set[Any] | frozenset[Any], spec: tuple[int, Any, bool]) -> None:
        ttype, tspec, _ = spec
        self.writeSetBegin(ttype, len(val))
        for _ in self._write_by_ttype(ttype, val, spec, tspec):
            pass
        self.writeSetEnd()

    def writeContainerMap(
        self, val: dict[Any, Any], spec: tuple[int, Any, int, Any, bool]
    ) -> None:
        ktype, kspec, vtype, vspec, _ = spec
        self.writeMapBegin(ktype, vtype, len(val))
        for _ in zip(
            self._write_by_ttype(ktype, val.keys(), spec, kspec),
            self._write_by_ttype(vtype, val.values(), spec, vspec),
        ):
            pass
        self.writeMapEnd()

    def writeStruct(self, obj: Any, thrift_spec: tuple[Any, ...]) -> None:
        self.writeStructBegin(obj.__class__.__name__)
        for field in thrift_spec:
            if field is None:
                continue
            fname = field[2]
            val = getattr(obj, fname)
            if val is None:
                # skip writing out unset fields
                continue
            fid = field[0]
            ftype = field[1]
            fspec = field[3]
            self.writeFieldBegin(fname, ftype, fid)
            self.writeFieldByTType(ftype, val, fspec)
            self.writeFieldEnd()
        self.writeFieldStop()
        self.writeStructEnd()

    def _write_by_ttype(
        self, ttype: int, vals: Iterable[Any], spec: Any, espec: Any
    ) -> Generator[Any, None, None]:
        _, writer_name, is_container = self._ttype_handlers(ttype, espec)
        assert writer_name is not None, f"No writer for ttype {ttype}"
        writer_func = getattr(self, writer_name)
        write = (lambda v: writer_func(v, espec)) if is_container else writer_func
        for v in vals:
            yield write(v)

    def writeFieldByTType(self, ttype: int, val: Any, spec: Any) -> None:
        next(self._write_by_ttype(ttype, [val], spec, spec))


def checkIntegerLimits(i: int, bits: int) -> None:
    if bits == 8 and (i < -128 or i > 127):
        raise TProtocolException(
            TProtocolException.INVALID_DATA, "i8 requires -128 <= number <= 127"
        )
    elif bits == 16 and (i < -32768 or i > 32767):
        raise TProtocolException(
            TProtocolException.INVALID_DATA, "i16 requires -32768 <= number <= 32767"
        )
    elif bits == 32 and (i < -2147483648 or i > 2147483647):
        raise TProtocolException(
            TProtocolException.INVALID_DATA,
            "i32 requires -2147483648 <= number <= 2147483647",
        )
    elif bits == 64 and (i < -9223372036854775808 or i > 9223372036854775807):
        raise TProtocolException(
            TProtocolException.INVALID_DATA,
            "i64 requires -9223372036854775808 <= number <= 9223372036854775807",
        )


class TProtocolFactory(ABC):
    @abstractmethod
    def getProtocol(self, trans: TTransportBase) -> TProtocolBase:
        pass
