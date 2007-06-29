-module(tApplicationException).

-include("thrift.hrl").
% -include("tApplicationException.hrl").

-export([new/0, new/1, new/2, read/2, write/2]).

new(Type, Message) ->
    #tApplicationException{type=Type, message=Message}.

new()     -> new(?tApplicationException_UNKNOWN, nil). % WATCH
new(Type) -> new(Type, nil). % WATCH

read(This, Iprot) ->
    ?M0(Iprot, readStructBegin),
    read_while_loop(This, Iprot),
    ?M0(Iprot, readStructEnd),
    This.

read_while_loop(This, Iprot) ->
    {_, Ftype, Fid} = ?M0(Iprot, readFieldBegin), % field = {fname, ftype, fid}

    if 
	Ftype == ?tType_STOP ->
	    This;
	(Fid == 1) and (Ftype == ?tType_STRING) ->
	    This1 = This#tApplicationException{message=?M0(Iprot, readString)},
	    ?M0(Iprot, readFieldEnd),
	    read_while_loop(This1, Iprot);

	Fid == 1 ->
	    ?M0(Iprot, skip),
	    ?M0(Iprot, readFieldEnd),
	    read_while_loop(This, Iprot);

	(Fid == 2) and (Ftype == ?tType_I32) ->
	    This1 = This#tApplicationException{type=?M0(Iprot, readI32)},
	    ?M0(Iprot, readFieldEnd),
	    read_while_loop(This1, Iprot);

        true -> 
	    ?M0(Iprot, skip),
	    ?M0(Iprot, readFieldEnd),
	    read_while_loop(This, Iprot)
    end.

write(This, Oprot) ->	
    ?M1(Oprot, writeStructBegin, "tApplicationException"),
    Message = This#tApplicationException.message,
    Type = This#tApplicationException.type,
    if	Message /= undefined ->
	    ?M3(Oprot, writeFieldBegin, "message", ?tType_STRING, 1),
	    ?M1(Oprot, writeString, Message),
	    ?M0(Oprot, writeFieldEnd);
        true -> ok
    end,
    if  Type /= undefined -> 
	    ?M3(Oprot, writeFieldBegin, "type", ?tType_I32, 2),
	    ?M1(Oprot, writeI32, Type),
	    ?M0(Oprot, writeFieldEnd);
        true -> ok
    end,
    ?M0(Oprot, writeFieldStop),
    ?M0(Oprot, writeStructEnd).
