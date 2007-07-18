%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tApplicationException).

-include("thrift.hrl").
-include("tApplicationException.hrl").

-include("oop.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/0, new/1, new/2, read/2, write/2]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super);
?DEFINE_ATTR(type).
   
%%%
%%% behavior callbacks
%%%

%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tException.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(type).

%%%
%%% class methods
%%%

new(Type, Message) ->
    Super = (super()):new(Message),
    #?MODULE{super=Super, type=Type}.

new()     -> new(?tApplicationException_UNKNOWN, undefined).
new(Type) -> new(Type, undefined).

%%%
%%% instance methods
%%%

read(This, Iprot) ->
    ?R0(Iprot, readStructBegin),
    read_while_loop(This, Iprot),
    ?R0(Iprot, readStructEnd),
    ok.

read_while_loop(This, Iprot) ->
    {_Fname, Ftype, Fid} = ?R0(Iprot, readFieldBegin),

    if 
	Ftype == ?tType_STOP ->
	    ok;

	(Fid == 1) and (Ftype == ?tType_STRING) ->
	    Message1 = ?R0(Iprot, readString),
	    This1 = oop:set(This, message, Message1),
	    ?R0(Iprot, readFieldEnd),
	    read_while_loop(This1, Iprot);

	Fid == 1 ->
	    ?R0(Iprot, skip),
	    ?R0(Iprot, readFieldEnd),
	    read_while_loop(This, Iprot);

	(Fid == 2) and (Ftype == ?tType_I32) ->
	    Type1 = ?R0(Iprot, readI32),
	    This1 = oop:set(This, type, Type1),
	    ?R0(Iprot, readFieldEnd),
	    read_while_loop(This1, Iprot);

        true -> 
	    ?R0(Iprot, skip),
	    ?R0(Iprot, readFieldEnd),
	    read_while_loop(This, Iprot)
    end.

write(This, Oprot) ->	
    ?R1(Oprot, writeStructBegin, "tApplicationException"),
    Message = oop:get(This, message),
    Type    = oop:get(This, type),

    if	Message /= undefined ->
	    ?R3(Oprot, writeFieldBegin, "message", ?tType_STRING, 1),
	    ?R1(Oprot, writeString, Message),
	    ?R0(Oprot, writeFieldEnd);
        true -> ok
    end,

    if  Type /= undefined -> 
	    ?R3(Oprot, writeFieldBegin, "type", ?tType_I32, 2),
	    ?R1(Oprot, writeI32, Type),
	    ?R0(Oprot, writeFieldEnd);
        true -> ok
    end,

    ?R0(Oprot, writeFieldStop),
    ?R0(Oprot, writeStructEnd),
    ok.
