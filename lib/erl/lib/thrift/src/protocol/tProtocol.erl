%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tProtocol).

-include("oop.hrl").

-include("thrift.hrl").
-include("protocol/tProtocol.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

%% -export([interface/1]).	  	             %%

-export([
  new/1,
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

%%%
%%% server interface
%%%

%% %%% modules we can instantiate from the server	       %%
%% interface(subclasses) -> 				       %%
%%     [						       %%
%%      tBinaryProtocol					       %%
%%     ];						       %%
%% 							       %%
%% %%% synchronous calls to pass			       %%
%% interface(call) -> 					       %%
%%     [						       %%
%%      skip,						       %%
%%      						       %%
%%      writeMessageBegin, writeMessageEnd,		       %%
%%      writeStructBegin, writeStructEnd,		       %%
%%      writeFieldBegin, writeFieldEnd, writeFieldStop,	       %%
%%      writeMapBegin, writeMapEnd,			       %%
%%      writeListBegin, writeListEnd,			       %%
%%      writeSetBegin, writeSetEnd,			       %%
%%      						       %%
%%      writeBool, writeByte, writeI16, writeI32, 	       %%
%%      writeI64, writeDouble, writeString, 		       %%
%%      						       %%
%%      readMessageBegin, readMessageEnd, 		       %%
%%      readStructBegin, readStructEnd, 		       %%
%%      readFieldBegin, readFieldEnd,			       %%
%%      readMapBegin, readMapEnd,			       %%
%%      readListBegin, readListEnd,			       %%
%%      readSetBegin, readSetEnd,			       %%
%%      						       %%
%%      readBool, readByte, readI16, readI32, 		       %%
%%      readI64, readDouble, readString			       %%
%%     ];						       %%
%% 							       %%
%% %%% asynchronous casts to pass			       %%
%% interface(cast) ->					       %%
%%     [].						       %%

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(trans).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    none.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(trans).

%%%
%%% class methods
%%%

new(Trans) ->
    #?MODULE{trans=Trans}.

%%%
%%% instance methods
%%%

writeMessageBegin(_This, _Name, _Type, _Seqid) -> ok.
writeMessageEnd(_This) -> ok.
writeStructBegin(_This, _Name) -> ok.
writeStructEnd(_This) -> ok.
writeFieldBegin(_This, _Name, _Type, _Id) -> ok.
writeFieldEnd(_This) -> ok.
writeFieldStop(_This) -> ok.
writeMapBegin(_This, _Ktype, _Vtype, _Size) -> ok.
writeMapEnd(_This) -> ok.
writeListBegin(_This, _Etype, _Size) -> ok.
writeListEnd(_This) -> ok.
writeSetBegin(_This, _Etype, _Size) -> ok.
writeSetEnd(_This) -> ok.

writeBool(_This, _Value) -> ok.
writeByte(_This, _Value) -> ok.
writeI16(_This, _Value) -> ok.
writeI32(_This, _Value) -> ok.
writeI64(_This, _Value) -> ok.
writeDouble(_This, _Value) -> ok.
writeString(_This, _Value) -> ok.

readMessageBegin(_This) -> ok.
readMessageEnd(_This) -> ok.
readStructBegin(_This) -> ok.
readStructEnd(_This) -> ok.
readFieldBegin(_This) -> ok.
readFieldEnd(_This) -> ok.
readMapBegin(_This) -> ok.
readMapEnd(_This) -> ok.
readListBegin(_This) -> ok.
readListEnd(_This) -> ok.
readSetBegin(_This) -> ok.
readSetEnd(_This) -> ok.

readBool(_This) -> ok.
readByte(_This) -> ok.
readI16(_This) -> ok.
readI32(_This) -> ok.
readI64(_This) -> ok.
readDouble(_This) -> ok.
readString(_This) -> ok.

skip(This, Type) ->
    case Type of 
	?tType_STOP   -> nil; % WATCH
	?tType_BOOL   -> ?L0(readBool);
	?tType_BYTE   -> ?L0(readByte);
	?tType_I16    -> ?L0(readI16);
	?tType_I32    -> ?L0(readI32);
	?tType_I64    -> ?L0(readI64);
	?tType_DOUBLE -> ?L0(readDouble);
	?tType_STRING -> ?L0(readString);

	?tType_STRUCT -> 
	    ?L0(readStructBegin),
	    skip_struct_loop(This),

	    %% cpiro: this isn't here in the original tprotocol.rb, but i think it's a bug
	    ?L0(readStructEnd);

	?tType_MAP ->
	    {Ktype, Vtype, Size} = ?L0(readMapBegin),
	    skip_map_repeat(This, Ktype, Vtype, Size),
	    ?L0(readMapEnd);

	?tType_SET -> 
	    {Etype, Size} = ?L0(readSetBegin),
	    skip_set_repeat(This, Etype, Size),
	    ?L0(readSetEnd);

	?tType_LIST ->
	    {Etype, Size} = ?L0(readListBegin),
	    skip_set_repeat(This, Etype, Size), % [sic] skipping same as for SET
	    ?L0(readListEnd)
    end.

skip_struct_loop(This) ->
    { _Name, Type, _Id } = ?L0(readFieldBegin),
    if
	Type == ?tType_STOP ->
	    ok;
	
	true ->
	    ?L1(skip, Type),
	    ?L0(readFieldEnd),

	    %% cpiro: this is here in original tprotocol.rb, but i think it's a bug
	    % ?L0(readStructEnd),
	    skip_struct_loop(This)
    end.
       
skip_map_repeat(This, Ktype, Vtype, Times) ->
    ?L1(skip, Ktype),
    ?L1(skip, Vtype),
    skip_map_repeat(This, Ktype, Vtype, Times-1).

skip_set_repeat(This, Etype, Times) ->
    ?L1(skip, Etype),
    skip_set_repeat(This, Etype, Times-1).
