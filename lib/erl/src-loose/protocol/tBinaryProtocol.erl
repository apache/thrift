-module(tBinaryProtocol).

-include("thrift/thrift.hrl").
-include("thrift/protocol/tBinaryProtocol.hrl").
-include("thrift/protocol/tProtocolException.hrl").

-export([
  new/1,
  trans/1,
  skip/2,

  writeMessageBegin/4, writeMessageEnd/1,
  writeStructBegin/2, writeStructEnd/1,
  writeFieldBegin/4, writeFieldEnd/1, writeFieldStop/1,
  writeMapBegin/4, writeMapEnd/1,
  writeListBegin/3, writeListEnd/1,
  writeSetBegin/3, writeSetEnd/1,

  writeBool/2, writeByte/2, writeI16/2, writeI32/2, 
  writeI64/2, writeDouble/2, writeString/2, 

  readMessageBegin/1, readMessageEnd/1, 
  readStructBegin/1, readStructEnd/1, 
  readFieldBegin/1, readFieldEnd/1,
  readMapBegin/1, readMapEnd/1,
  readListBegin/1, readListEnd/1,
  readSetBegin/1, readSetEnd/1,

  readBool/1, readByte/1, readI16/1, readI32/1, 
  readI64/1, readDouble/1, readString/1
]).

new(Trans) ->
    #tBinaryProtocol{trans=Trans}.

trans(This) -> % accessor
    ?ATTR(trans).

skip(This, Type) ->
    tProtocol:skip(This, Type).

writeMessageBegin(This, Name, Type, Seqid) ->
    writeI32(This, ?VERSION_1 bor Type),
    writeString(This, Name),
    writeI32(This, Seqid).

writeMessageEnd(This) ->
    This, % suppress unused warnings
    ok.

writeStructBegin(This, Name) ->
    This, Name, % suppress unused warnings
    ok.

writeStructEnd(This) ->
    This, % suppress unused warnings
    ok.

writeFieldBegin(This, Name, Type, Id) ->
    Name,
    writeByte(This, Type),
    writeI16(This, Id).

writeFieldEnd(This) ->
    This, % suppress unused warnings
    ok.

writeFieldStop(This) ->
    writeByte(This, ?tType_STOP).

writeMapBegin(This, Ktype, Vtype, Size) ->
    writeByte(This, Ktype),
    writeByte(This, Vtype),
    writeI32(This, Size).

writeMapEnd(This) ->
    This, % suppress unused warnings
    ok.

writeListBegin(This, Etype, Size) ->
    writeByte(This, Etype),
    writeI32(This, Size).

writeListEnd(This) ->
    This, % suppress unused warnings
    ok.

writeSetBegin(This, Etype, Size) ->
    writeByte(This, Etype),
    writeI32(This, Size).

writeSetEnd(This) ->
    This, % suppress unused warnings
    ok.

%

writeBool(This, Bool) ->
    if Bool -> % true
	    writeByte(This, 1);
       true -> % false
	    writeByte(This, 0)
    end.

writeByte(This, Byte) ->
    Trans = This#tBinaryProtocol.trans,
    ?M1(Trans, write, binary_to_list(<<Byte:8/big>>)).

writeI16(This, I16) ->
    Trans = This#tBinaryProtocol.trans,
    ?M1(Trans, write, binary_to_list(<<I16:16/big>>)).

writeI32(This, I32) ->
    Trans = This#tBinaryProtocol.trans,
    ?M1(Trans, write, binary_to_list(<<I32:32/big>>)).

writeI64(This, I64) ->
    Trans = This#tBinaryProtocol.trans,
    ?M1(Trans, write, binary_to_list(<<I64:64/big>>)).

writeDouble(This,  Double) ->
    Trans = This#tBinaryProtocol.trans,
    ?M1(Trans, write, binary_to_list(<<Double:64/big>>)).

writeString(This, Str) ->
    Trans = This#tBinaryProtocol.trans,
    writeI32(This, length(Str)),
    ?M1(Trans, write, Str).

%

readMessageBegin(This) ->
    Version = readI32(This),
    if 
	(Version band ?VERSION_MASK) /= ?VERSION_1 ->
	    throw(tProtocolException:new(?tProtocolException_BAD_VERSION,
					 "Missing version identifier"));
	true -> ok
    end,
    Type = Version band 16#000000ff,
    Name  = readString(This),
    Seqid = readI32(This),
    { Name, Type, Seqid }.

readMessageEnd(This) ->
    This, % suppress unused warnings
    ok.

readStructBegin(This) ->
    This, % suppress unused warnings
    ok.

readStructEnd(This) ->
    This, % suppress unused warnings
    ok.

readFieldBegin(This) ->
    Type = readByte(This),
    if Type == ?tType_STOP ->
	    { nil, Type, 0 }; % WATCH
       true ->
	    Id = readI16(This),
	    { nil, Type, Id }
    end.

readFieldEnd(This) ->
    This, % suppress unused warnings
    ok.

readMapBegin(This) ->
    Ktype = readByte(This),
    Vtype = readByte(This),
    Size  = readI32(This),
    { Ktype, Vtype, Size }.

readMapEnd(This) ->
    This, % suppress unused warnings
    ok.

readListBegin(This) ->
    Etype = readByte(This),
    Size  = readI32(This),
    { Etype, Size }.

readListEnd(This) ->
    This, % suppress unused warnings
    ok.

readSetBegin(This) ->
    Etype = readByte(This),
    Size  = readI32(This),
    { Etype, Size }.

readSetEnd(This) ->
    This, % suppress unused warnings
    ok.

% WATCH everything ... who knows what of this will work

readBool(This) ->
    Byte = readByte(This),
    (Byte /= 0).

readByte(This) ->
    Trans = This#tBinaryProtocol.trans,
    <<Val:8/integer-signed-big, _/binary>>  = ?M1(Trans, readAll, 1),
    Val.

readI16(This) ->
    Trans = This#tBinaryProtocol.trans,
    <<Val:16/integer-signed-big, _/binary>>  = ?M1(Trans, readAll, 2),
    Val.

readI32(This) ->
    Trans = This#tBinaryProtocol.trans,
    <<Val:32/integer-signed-big, _/binary>>  = ?M1(Trans, readAll, 4),
    Val.

readI64(This) ->
    Trans = This#tBinaryProtocol.trans,
    <<Val:64/integer-signed-big, _/binary>>  = ?M1(Trans, readAll, 8),
    Val.

readDouble(This) ->
    Trans = This#tBinaryProtocol.trans,
    <<Val:64/float-signed-big, _/binary>>  = ?M1(Trans, readAll, 8),
    Val.

readString(This) ->
    Trans = This#tBinaryProtocol.trans,
    Sz    = readI32(This),
    binary_to_list(?M1(Trans, readAll, Sz)).
