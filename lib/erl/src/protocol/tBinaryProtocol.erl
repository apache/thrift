%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tBinaryProtocol).

-include("oop.hrl").

-include("thrift.hrl").
-include("protocol/tProtocolException.hrl").
-include("protocol/tBinaryProtocol.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([
  new/1,

  writeMessageBegin/4,
  writeFieldBegin/4, writeFieldStop/1,
  writeMapBegin/4,
  writeListBegin/3,
  writeSetBegin/3,

  writeBool/2, writeByte/2, writeI16/2, writeI32/2,
  writeI64/2, writeDouble/2, writeString/2,

  readMessageBegin/1,
  readFieldBegin/1,
  readMapBegin/1,
  readListBegin/1,
  readSetBegin/1,

  readBool/1, readByte/1, readI16/1, readI32/1,
  readI64/1, readDouble/1, readString/1
]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super).

%%%
%%% behavior callbacks
%%%

%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tProtocol.

%%% inspect(This) -> string()

inspect(_This) ->
    "".

%%%
%%% class methods
%%%

new(Trans) ->
    Super = (super()):new(Trans),
    #?MODULE{super=Super}.

%%%
%%% instance methods
%%%

writeMessageBegin(This, Name, Type, Seqid) ->
    ?L1(writeI32, ?VERSION_1 bor Type),
    ?L1(writeString, Name),
    ?L1(writeI32, Seqid),
    ok.

writeFieldBegin(This, _Name, Type, Id) ->
    ?L1(writeByte, Type),
    ?L1(writeI16, Id),
    ok.

writeFieldStop(This) ->
    ?L1(writeByte, ?tType_STOP),
    ok.

writeMapBegin(This, Ktype, Vtype, Size) ->
    ?L1(writeByte, Ktype),
    ?L1(writeByte, Vtype),
    ?L1(writeI32, Size),
    ok.

writeListBegin(This, Etype, Size) ->
    ?L1(writeByte, Etype),
    ?L1(writeI32, Size),
    ok.

writeSetBegin(This, Etype, Size) ->
    ?L1(writeByte, Etype),
    ?L1(writeI32, Size),
    ok.

%

writeBool(This, Bool) ->
    case Bool of
        true  -> ?L1(writeByte, 1);
        false -> ?L1(writeByte, 0)
    end.

writeByte(This, Byte) ->
    Trans = oop:get(This, trans),
    ?R1(Trans, effectful_write, binary_to_list(<<Byte:8/big>>)).

writeI16(This, I16) ->
    Trans = oop:get(This, trans),
    ?R1(Trans, effectful_write, binary_to_list(<<I16:16/big>>)).

writeI32(This, I32) ->
    Trans = oop:get(This, trans),
    ?R1(Trans, effectful_write, binary_to_list(<<I32:32/big>>)).

writeI64(This, I64) ->
    Trans = oop:get(This, trans),
    ?R1(Trans, effectful_write, binary_to_list(<<I64:64/big>>)).

writeDouble(This, Double) ->
    Trans = oop:get(This, trans),
    ?R1(Trans, effectful_write, binary_to_list(<<Double:64/big>>)).

writeString(This, Str) ->
    Trans = oop:get(This, trans),
    ?L1(writeI32, length(Str)),
    ?R1(Trans, effectful_write, Str).

%

readMessageBegin(This) ->
    Version = ?L0(readI32),
    if
        (Version band ?VERSION_MASK) /= ?VERSION_1 ->
            throw(tProtocolException:new(?tProtocolException_BAD_VERSION,
                                         "Missing version identifier"));
        true -> ok
    end,
    Type = Version band 16#000000ff,
    Name  = ?L0(readString),
    Seqid = ?L0(readI32),
    { Name, Type, Seqid }.

readFieldBegin(This) ->
    Type = ?L0(readByte),
    case Type of
        ?tType_STOP ->
            { nil, Type, 0 }; % WATCH
        _ ->
            Id = ?L0(readI16),
            { nil, Type, Id }
    end.

readMapBegin(This) ->
    Ktype = ?L0(readByte),
    Vtype = ?L0(readByte),
    Size  = ?L0(readI32),
    { Ktype, Vtype, Size }.

readListBegin(This) ->
    Etype = ?L0(readByte),
    Size  = ?L0(readI32),
    { Etype, Size }.

readSetBegin(This) ->
    Etype = ?L0(readByte),
    Size  = ?L0(readI32),
    { Etype, Size }.

%

readBool(This) ->
    Byte = ?L0(readByte),
    (Byte /= 0).

readByte(This) ->
    Trans = oop:get(This, trans),
    <<Val:8/integer-signed-big, _/binary>>  = ?R1(Trans, readAll, 1),
    Val.

readI16(This) ->
    Trans = oop:get(This, trans),
    <<Val:16/integer-signed-big, _/binary>>  = ?R1(Trans, readAll, 2),
    Val.

readI32(This) ->
    Trans = oop:get(This, trans),
    <<Val:32/integer-signed-big, _/binary>>  = ?R1(Trans, readAll, 4),
    Val.

readI64(This) ->
    Trans = oop:get(This, trans),
    <<Val:64/integer-signed-big, _/binary>>  = ?R1(Trans, readAll, 8),
    Val.

readDouble(This) ->
    Trans = oop:get(This, trans),
    <<Val:64/float-signed-big, _/binary>>  = ?R1(Trans, readAll, 8),
    Val.

readString(This) ->
    Trans = oop:get(This, trans),
    Sz    = ?L0(readI32),
    binary_to_list(?R1(Trans, readAll, Sz)).
