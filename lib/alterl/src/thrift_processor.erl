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

    try
        {Micro, Result} = better_timer(Handler, handle_function, [Function, Params]),
        error_logger:info_msg("Processed ~p(~p) in ~.4fms~n",
                              [Function, Params, Micro/1000.0]),
        handle_success(State, Function, Result)
    catch
        throw:Exception when is_tuple(Exception), size(Exception) > 0 ->
            error_logger:warning_msg("~p threw exception: ~p~n", [Function, Exception]),
            handle_exception(State, Function, Exception),
            ok;   % we still want to accept more requests from this client
        error:Error ->
            ok = handle_error(State, Function, Error)
    end.

handle_success(State = #state{out_protocol = OProto,
                              service = Service},
               Function,
               Result) ->
    ReplyType = Service:function_info(Function, reply_type),
    StructName = atom_to_list(Function) ++ "_result",
    
    case Result of
        {reply, ReplyData} -> 
            Reply = {{struct, [{0, ReplyType}]}, {StructName, ReplyData}},
            ok = send_reply(OProto, Function, ?tMessageType_REPLY, Reply);

        ok when ReplyType == {struct, []} ->
            ok = send_reply(OProto, Function, ?tMessageType_REPLY, {ReplyType, {StructName}})
    end,
    ok.

handle_exception(State = #state{out_protocol = OProto,
                                service = Service},
                 Function,
                 Exception) ->
    ExceptionType = element(1, Exception),
    % Fetch a structure like {struct, [{-2, {struct, {Module, Type}}},
    %                                  {-3, {struct, {Module, Type}}}]}

    ReplySpec = Service:function_info(Function, exceptions),
    {struct, XInfo} = ReplySpec,

    true = is_list(XInfo),
    
    % e.g.: [{-1, type0}, {-2, type1}, {-3, type2}]
    XPairs = [{Fid, Type} || {Fid, {struct, {_Module, Type}}} <- XInfo],

    Mapper = fun({Fid, Type}) ->
                     case Type of
                         ExceptionType ->
                             Exception;
                         _ ->
                             undefined
                     end
             end,
    % Assuming we had a type1 exception, we get: [undefined, Exception, undefined]
    ExceptionList = lists:map(Mapper, XPairs),
    ExceptionTuple = list_to_tuple([Function | ExceptionList]),
    
    % Make sure we got at least one defined
    case lists:all(fun(X) -> X =:= undefined end, ExceptionList) of
        true ->
            ok = handle_unknown_exception(State, Function, Exception);
        false ->
            ok = send_reply(OProto, Function, ?tMessageType_REPLY, {ReplySpec, ExceptionTuple})
    end.

%%
% Called when an exception has been explicitly thrown by the service, but it was
% not one of the exceptions that was defined for the function.
%%
handle_unknown_exception(State, Function, Exception) ->
    handle_error(State, Function, {exception_not_declared_as_thrown,
                                   Exception}).

handle_error(#state{out_protocol = OProto}, Function, Error) ->
    Stack = erlang:get_stacktrace(),
    error_logger:error_msg("~p had an error: ~p~n", [Function, {Error, Stack}]),
    
    Message =
        case application:get_env(thrift, exceptions_include_traces) of
            {ok, true} ->
                lists:flatten(io_lib:format("An error occurred: ~p~n",
                                            [{Error, Stack}]));
            _ ->
                "An unknown handler error occurred."
        end,
    Reply = {?TApplicationException_Structure,
             #'TApplicationException'{
                message = Message,
                type = ?TApplicationException_UNKNOWN}},
    send_reply(OProto, Function, ?tMessageType_EXCEPTION, Reply).


send_reply(OProto, Function, ReplyMessageType, Reply) ->
    ok = thrift_protocol:write(OProto, #protocol_message_begin{
                                 name = atom_to_list(Function),
                                 type = ReplyMessageType,
                                 seqid = 0}),
    ok = thrift_protocol:write(OProto, Reply),
    ok = thrift_protocol:write(OProto, message_end),
    ok = thrift_protocol:flush_transport(OProto),
    ok.


%%
% This is the same as timer:tc except that timer:tc appears to catch
% exceptions when it shouldn't!
%%
better_timer(Module, Function, Args) ->
    T1 = erlang:now(),
    Result = apply(Module, Function, Args),
    T2 = erlang:now(),
    {timer:now_diff(T2, T1), Result}.
