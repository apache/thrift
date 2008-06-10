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
    MessageBegin = thrift_protocol:read(IProto, message_begin),
    io:format("Got message begin: ~p~n", [MessageBegin]),

    [ok, ok, ok, ok] = [thrift_protocol:read(IProto, X)
                    || X <- [struct_begin, field_stop, struct_end, message_end]],
    io:format("Read everything okay!"),

    Packets =
        [
         #protocol_message_begin{name = "getServiceStatus",
                                 type = ?tMessageType_REPLY,
                                 seqid = 0},
         struct_begin,
         #protocol_field_begin{name = "success",
                               type = ?tType_MAP,
                               id = 0},
         #protocol_map_begin{ktype = ?tType_STRING,
                             vtype = ?tType_STRING,
                             size  = 2},
         {string, "Hello"},
         {string, "World"},
         {string, "foo"},
         {string, "bar"},
         field_stop,
         map_end,
         field_end,
         field_stop,
         struct_end,
         message_end
         ],
               
    Results = [thrift_protocol:write(OProto, Packet) || Packet <- Packets],
    receive
        _ ->
            loop(State)
    end.
