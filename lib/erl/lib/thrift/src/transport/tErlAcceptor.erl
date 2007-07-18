%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tErlAcceptor).

-include("oop.hrl").
-include("thrift.hrl").
-include("transport/tTransportException.hrl").
-include("transport/tServerSocket.hrl").
-include("transport/tErlAcceptor.hrl").

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
    io:format("acceptor started~n",[]),
    
    ServerPid = oop:get(This, serverPid),
    
    case catch gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    
	    ?C0(ServerPid, effectful_new_acceptor), %% cast to create new acceptor

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

	    receive_loop(This, Processor, Prot, Prot),
	    
	    exit(acceptor_done); %% TODO(cpiro): grace?

	Else ->
	    R = lists:flatten(
	      io_lib:format("accept() failed: ~p", [Else])),
    
	    exit(tTransportException:new(R))
    end.

receive_loop(This, Processor, Iprot, Oprot) ->
    case catch ?R2(Processor, process, Iprot, Oprot) of 
	{'EXIT', X} -> 
	    io:format("Acceptor: we gotta ~p~n", [X]);

	Value ->
	    io:format("request processed: rv=~p~n", [Value]),
	    receive_loop(This, Processor, Iprot, Oprot)
    end.

%%%
%%% error handlers
%%%

%% end

%%% old codez

%% effectful_listen(This) ->								        %%
%%     Port = oop:get(This, port),							        %%
%%     Options = [binary, {packet, 0}, {active, false}], % was []			        %%
%% 											        %%
%%     case gen_tcp:listen(Port, Options) of 						        %%
%% 	{ok, ListenSocket} ->								        %%
%% 	    This1 = oop:set(This, handle, ListenSocket),				        %%
%% 	    {ok, This1}									        %%
%%     											        %%
%% 	% {error, _} -> 								        %%
%% 	% TODO: no error handling in Ruby version?					        %%
%%     end.										        %%
%% 											        %%
%% accept(This) ->									        %%
%%     case oop:get(This, handle) of 							        %%
%% 	nil ->										        %%
%% 	    nil; % cpiro: sic the Ruby code						        %%
%% 											        %%
%% 	Handle ->									        %%
%% 	    case gen_tcp:accept(Handle) of						        %%
%% 		{ok, Sock} ->								        %%
%% 		    Trans = oop:start_new(tSocket, []),					        %%
%% 		    ?R1(Trans, effectful_setHandle, Sock),				        %%
%% 		    Trans								        %%
%% 	        % {error, _} -> 							        %%
%%                 % TODO: no error handling in Ruby version?				        %%
%% 	    end										        %%
%%     end.										        %%
%% 											        %%
%% effectful_close(This) ->								        %%
%%     case oop:get(This, handle) of 							        %%
%% 	nil ->										        %%
%% 	    {nil, This};								        %%
%% 	Handle ->									        %%
%% 	    case gen_tcp:close(Handle) of 						        %%
%% 		ok ->									        %%
%% 		    {ok, This} % cpiro: sic the Ruby version: don't set handle to nil	        %%
%% 	        % {error, _} -> 							        %%
%%                 % TODO: no error handling in Ruby version?				        %%
%% 	    end										        %%
%%     end.										        %%


%%% teh iservez

%% -module(iserve_socket).											 %%
%% 														 %%
%% -export([start_link/3]).											 %%
%% 														 %%
%% -export([init/1]).												 %%
%% -include("iserve.hrl").											 %%
%% 														 %%
%% %TEST													 %%
%% -export([handle_get/2]).											 %%
%% 														 %%
%% -define(not_implemented_501, "HTTP/1.1 501 Not Implemented\r\n\r\n").					 %%
%% -define(forbidden_403, "HTTP/1.1 403 Forbidden\r\n\r\n").							 %%
%% -define(not_found_404, "HTTP/1.1 404 Not Found\r\n\r\n").							 %%
%% 														 %%
%% -record(c,  {sock,												 %%
%%              port,												 %%
%%              peer_addr,											 %%
%%              peer_port											 %%
%% 	     }).												 %%
%% 														 %%
%% -define(server_idle_timeout, 30*1000).									 %%
%% 														 %%
%% start_link(ListenPid, ListenSocket, ListenPort) ->								 %%
%%     proc_lib:spawn_link(?MODULE, init, [{ListenPid, ListenSocket, ListenPort}]).				 %%
%% 														 %%

  
%% init({Listen_pid, Listen_socket, ListenPort}) ->								 %%
%%     % error_logger:info_msg("Socket Started~n"),								 %%
%%     case catch gen_tcp:accept(Listen_socket) of								 %%
%% 	{ok, Socket} ->												 %%
%%             %% Send the cast message to the listener process to create a new acceptor			 %%
%% 	    iserve_server:create(Listen_pid, self()),								 %%
%% 	    {ok, {Addr, Port}} = inet:peername(Socket),								 %%
%%             Conn = #c{sock = Socket,										 %%
%%                       port = ListenPort,									 %%
%%                       peer_addr = Addr,									 %%
%%                       peer_port = Port},									 %%
%% 	    request(Conn, #req{}); %% Jump to state 'request'							 %%
%% 	Else ->													 %%
%% 	    error_logger:error_report([{application, iserve},							 %%
%% 				       "Accept failed error",							 %%
%% 				       io_lib:format("~p",[Else])]),						 %%
%% 	    exit({error, accept_failed})									 %%
%%     end.													 %%
%% 														 %%

%% request(Conn, Req) ->											 %%
%%     case gen_tcp:recv(Conn#c.sock, 0, ?server_idle_timeout) of						 %%
%%         {ok, {http_request,Method,Path,Version}} ->								 %%
%%             headers(Conn, Req#req{vsn = Version,								 %%
%%                                   method = Method,								 %%
%%                                   uri = Path}, []);								 %%
%%         {error, {http_error, "\r\n"}} ->									 %%
%% 	    request(Conn, Req);											 %%
%% 	{error, {http_error, "\n"}} ->										 %%
%%             request(Conn, Req);										 %%
%%         {tcp_closed, _Port} ->										 %%
%%             error_logger:info_msg("Closed connection: ~p ~p~n", [Conn#c.peer_addr, Conn#c.peer_port]),	 %%
%%             exit(normal);											 %%
%% 	_Other ->												 %%
%% 	    exit(normal)											 %%
%%     end.													 %%

