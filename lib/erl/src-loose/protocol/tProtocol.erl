-module(tProtocol).

-include("thrift/thrift.hrl").
-include("thrift/protocol/tProtocol.hrl").

-export([new/1, skip/2]).

skip_struct_loop(This) ->
    { Name, Type, Id } = ?M0(This, readFieldBegin),
    Name, Id, % suppress unused warnings
    if
	Type == ?tType_STOP ->
	    ok;
	true ->
	    skip(This, Type),
	    ?M0(This, readFieldEnd),

	    %% this is here in original tprotocol.rb, but i think it's a bug
	    % ?M0(This, readStructEnd),
	    skip_struct_loop(This)
    end.
       
skip_map_repeat(This, Ktype, Vtype, Times) ->
    skip(This, Ktype),
    skip(This, Vtype),
    skip_map_repeat(This, Ktype, Vtype, Times-1).

skip_set_repeat(This, Etype, Times) ->
    skip(This, Etype),
    skip_set_repeat(This, Etype, Times-1).

new(Trans) ->
    #tProtocol{trans=Trans}.

skip(This, Type) ->
    case Type of 
	?tType_STOP   -> nil; % WATCH
	?tType_BOOL   -> ?M0(This, readBool);
	?tType_BYTE   -> ?M0(This, readByte);
	?tType_I16    -> ?M0(This, readI16);
	?tType_I32    -> ?M0(This, readI32);
	?tType_I64    -> ?M0(This, readI64);
	?tType_DOUBLE -> ?M0(This, readDouble);
	?tType_STRING -> ?M0(This, readString);

	?tType_STRUCT -> 
	    ?M0(This, readStructBegin),
	    skip_struct_loop(This),

	    %% this isn't here in the original tprotocol.rb, but i think it's a bug
	    ?M0(This, readStructEnd);

	?tType_MAP ->
	    {Ktype, Vtype, Size} = ?M0(This, readMapBegin),
	    skip_map_repeat(This, Ktype, Vtype, Size),
	    ?M0(This, readMapEnd);

	?tType_SET -> 
	    {Etype, Size} = ?M0(This, readSetBegin),
	    skip_set_repeat(This, Etype, Size),
	    ?M0(This, readSetEnd);

	?tType_LIST ->
	    {Etype, Size} = ?M0(This, readListBegin),
	    skip_set_repeat(This, Etype, Size), % [sic] skipping same as for SET
	    ?M0(This, readListEnd)
    end.


