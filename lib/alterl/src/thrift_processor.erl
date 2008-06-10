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
            ok = handle_function(State, list_to_atom(binary_to_list(Function))),
            loop(State);
        {error, closed} ->
            error_logger:info_msg("Processor finished~n"),
            ok
    end.

handle_function(State = #state{in_protocol = IProto,
                               out_protocol = OProto},
                add) ->
    io:format("Reading struct~n"),
    {ok, Struct} = thrift_protocol:read(IProto,
                  {struct, [{1, i32},
                            {2, i32}]}),
    io:format("Struct: ~p~n", [Struct]),
    
    {A, B} = Struct,

    thrift_protocol:write(OProto, #protocol_message_begin{
                            name = "addResult",
                            type = ?tMessageType_REPLY,
                            seqid = 0}),
    thrift_protocol:write(OProto, {{struct, [{0, i32}]},
                                   {A + B}}),
    thrift_protocol:write(OProto, message_end);

handle_function(State = #state{in_protocol = IProto,
                               out_protocol = OProto},
                complexTest) ->
    io:format("Reading struct~n"),
    Struct = thrift_protocol:read(
               IProto,
               {struct, [{1, {struct,
                              [{1, {list, i32}},
                               {2, {map, string, {struct, [{1, i16}]}}}]}}]}),
    
    io:format("Struct: ~p~n", [Struct]).


