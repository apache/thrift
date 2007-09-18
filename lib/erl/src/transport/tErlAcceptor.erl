%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tErlAcceptor).

-include("oop.hrl").
-include("thrift.hrl").
-include("tApplicationException.hrl").
-include("transport/tTransportException.hrl").
-include("transport/tServerSocket.hrl").
-include("transport/tErlAcceptor.hrl").

-include_lib("kernel/include/inet.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/3, accept/4]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super);
?DEFINE_ATTR(serverPid);
?DEFINE_ATTR(transportFactory);
?DEFINE_ATTR(protocolFactory).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tServerTransport.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(serverPid) ++ ", " ++
    ?FORMAT_ATTR(transportFactory) ++ ", " ++
    ?FORMAT_ATTR(protocolFactory).

%%%
%%% class methods
%%%

new(ServerPid, TF, PF) ->
    Super = (super()):new(),
    #?MODULE{super = Super, 
	     serverPid = ServerPid,
	     transportFactory = TF,
	     protocolFactory = PF
	    }.

%%%
%%% instance methods
%%%

accept(This, ListenSocket, GP, Handler) ->
    ServerPid = oop:get(This, serverPid),

    case catch gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    ?C0(ServerPid, effectful_new_acceptor), %% cast to create new acceptor

	    AddrString = render_addr(Socket),
	    ?INFO(conn_accepted, {AddrString}),

	    %% start_new(tSocket, [])
	    Client = oop:start_new(tSocket, []),
	    ?R1(Client, effectful_setHandle, Socket), %% TODO(cpiro): should we just let this be a param to the constructor?

	    %% cpiro: OPAQUE!! Trans = Client
	    TF      = oop:get(This, transportFactory),
	    Trans   = ?F1(TF, getTransport, Client), 

	    %% cpiro: OPAQUE!! Prot = start_new(tBinaryProtocol, [Trans])
	    PF      = oop:get(This, protocolFactory),
	    Prot    = ?F1(PF, getProtocol, Trans), 

	    %% start_new(, ...)
	    Processor = oop:start_new(tErlProcessor, [GP, Handler]), %% TODO

	    case receive_loop(This, Processor, Prot, Prot) of
		conn_timeout ->
		    ?INFO(conn_timeout, {AddrString});
		conn_closed ->
		    ?INFO(conn_closed, {AddrString});
		{Class, Else} ->
		    ?ERROR("unhandled ~p in tErlAcceptor: ~p", [Class, Else])
	    end,
	    exit(normal);

	Else ->
	    R = thrift_utils:sformat("accept() failed: ~p", [Else]),
	    exit(tTransportException:new(R))
    end.

receive_loop(This, Processor, Iprot, Oprot) ->
    try ?R2(Processor, process, Iprot, Oprot) of
	{error, TAE} when is_record(TAE, tApplicationException),
	                  TAE#tApplicationException.type == ?tApplicationException_HANDLER_ERROR ->
	    ?ERROR("handler returned an error: ~p", [oop:get(TAE, message)]),
	    receive_loop(This, Processor, Iprot, Oprot);
	Value ->
	    ?INFO(req_processed, {Value}),
	    receive_loop(This, Processor, Iprot, Oprot)
    catch
	exit:{timeout, _} ->
	    conn_timeout;

	%% the following clause must be last
	%% cpiro: would be best to implement an is_a/2 guard BIF
	%% cpiro: breaks if it's a subclass of tTransportException
	%% since unnest_record knows nothing about oop
	Class:Else ->
	    case thrift_utils:unnest_record(Else, tTransportException) of
		{ok, TTE} when TTE#tTransportException.type == ?tTransportException_NOT_OPEN ->
		    conn_closed;
		_ ->
		    {Class, Else}
	    end
    end.

%% helper functions

%% @param Socket the socket in question
%% TODO(cpiro): there probably needs to be a switch for DoLookup somewhere prominent and outside the lib,
%% probably at the "application" level
render_addr(Socket) ->
    DoLookup = true,
    {ok, {Peer, Port}} = inet:peername(Socket),

    case Peer of
	_ when DoLookup ->
	    case catch inet:gethostbyaddr(Peer) of
		{ok, Hostent} ->
		    thrift_utils:sformat("~s:~p", [Hostent#hostent.h_name, Port]);
		_ ->
		    "??"
	    end;

	{A,B,C,D} when not DoLookup ->
	    thrift_utils:sformat("~p.~p.~p.~p:~p", [A,B,C,D,Port])
    end.
