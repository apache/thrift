%%%-------------------------------------------------------------------
%%% File    : thrift_processor.erl
%%% Author  :  <todd@lipcon.org>
%%% Description : 
%%%
%%% Created : 28 Jan 2008 by  <todd@lipcon.org>
%%%-------------------------------------------------------------------
-module(thrift_processor).

-export([start/4,init/4]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-record(state, {handler, in_protocol, out_protocol, service}).

start(IProt, OProt, Service, Handler) ->
    spawn(thrift_processor, init, [IProt, OProt, Service, Handler]).

init(IProt, OProt, Service, Handler) ->
    io:format("Processor started~n"),
    loop(#state{in_protocol = IProt,
                out_protocol = OProt,
                service = Service,
                handler = Handler}).

loop(State = #state{in_protocol = IProto,
                    out_protocol = OProto}) ->
    case thrift_protocol:read(IProto, message_begin) of
        #protocol_message_begin{name = Function,
                                type = ?tMessageType_CALL} ->
            ok= handle_function(State, list_to_atom(Function)),
            loop(State);
        {error, closed} ->
            error_logger:info_msg("Client disconnected~n"),
            ok
    end.


handle_function(State = #state{in_protocol = IProto,
                               out_protocol = OProto,
                               handler = Handler,
                               service = Service},
                Function) ->
    InParams = Service:function_info(Function, params_type),

    {ok, Params} = thrift_protocol:read(IProto, InParams),

    {Micro, Result} = timer:tc(Handler, handle_function, [Function, Params]),
    error_logger:info_msg("Processed ~p(~p) in ~.4fms~n",
                          [Function, Params, Micro/1000.0]),
    
    ReplyType = Service:function_info(Function, reply_type),
    StructName = atom_to_list(Function) ++ "_result",
    
    case Result of
        {reply, ReplyData} -> 
            Reply = {{struct, [{0, ReplyType}]}, {StructName, ReplyData}},
            ok = send_reply(OProto, Function, Reply);

        ok when ReplyType == {struct, []} ->
            ok = send_reply(OProto, Function, {ReplyType, {StructName}})
    end,
    ok.


send_reply(OProto, Function, Reply) ->
    ok = thrift_protocol:write(OProto, #protocol_message_begin{
                                 name = atom_to_list(Function),
                                 type = ?tMessageType_REPLY,
                                 seqid = 0}),
    ok = thrift_protocol:write(OProto, Reply),
    ok = thrift_protocol:write(OProto, message_end),
    ok.
