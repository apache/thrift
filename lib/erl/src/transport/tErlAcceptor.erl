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
-include("protocol/tProtocolException.hrl").
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
            ?C0(ServerPid, effectful_new_acceptor), % cast to create new acceptor

            AddrString = render_addr(Socket),
            ?INFO("thrift connection accepted from ~s", [AddrString]),

            Client = oop:start_new(tSocket, []),
            ?R1(Client, effectful_setHandle, Socket),

            %% cpiro: OPAQUE!! Trans = Client
            TF      = oop:get(This, transportFactory),
            Trans   = ?F1(TF, getTransport, Client),

            %% cpiro: OPAQUE!! Prot = start_new(tBinaryProtocol, [Trans])
            PF      = oop:get(This, protocolFactory),
            Prot    = ?F1(PF, getProtocol, Trans),

            %% start_new(, ...)
            Processor = oop:start_new(tErlProcessor, [GP, Handler]),

            try
                receive_loop(This, Processor, Prot, Prot)
            catch
                exit:{timeout, _} ->
                    ?INFO("thrift connection timed out from ~s", [AddrString]);

                %% cpiro: i think the extra entry on the stack is always from receive_loop
                %% the below case shouldn't happen then?  if we move this catch inside
                %% we'll probably need this case and not the next one

                %% exit:{thrift_exception, E} ->
                %%     handle_exception(E, AddrString, no2);

                exit:{{thrift_exception, E}, Stack1} ->
                    handle_exception(E, AddrString, Stack1);

                Class:Else ->
                    ?ERROR("some other error ~p in tErlAcceptor: ~p", [Class, Else])
            end,
            exit(normal);

        Else ->
            R = thrift_utils:sformat("accept() failed: ~p", [Else]),
            tException:throw(tTransportException, [R])
    end.


handle_exception(E, AddrString, Stack1) ->
    case tException:read(E) of
        none -> % not a tException
            ?ERROR("not really a tException: ~p", [exit, E]);

        {tProtocolException, ?tProtocolException_BAD_VERSION, _} ->
            ?INFO("thrift missing version from ~s", [AddrString]);

        {tTransportException, ?tTransportException_NOT_OPEN, _} ->
            ?INFO("thrift connection closed from ~s", [AddrString]);

        _ ->
            Where = "thrift tErlAcceptor caught a tException",
            ?ERROR("~s", [tException:inspect_with_backtrace(E, Where, Stack1)])
    end.

%% always calls itself ... only way to escape is through an exit
receive_loop(This, Processor, Iprot, Oprot) ->
    case ?R2(Processor, process, Iprot, Oprot) of
        {error, Reason} ->
            case tException:read(Reason) of
                none ->
                    ?ERROR("thrift handler returned something weird: {error, ~p}", [Reason]);
                _ ->
                    Where = "thrift processor/handler caught a tException",
                    ?ERROR("~s", [tException:inspect_with_backtrace(Reason, Where)])
            end,
            receive_loop(This, Processor, Iprot, Oprot);
        Value ->
            ?INFO("thrift request: ~p", [Value]),
            receive_loop(This, Processor, Iprot, Oprot)
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
