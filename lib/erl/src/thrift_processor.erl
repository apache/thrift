%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(thrift_processor).

-export([init/1]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-record(thrift_processor, {handler, in_protocol, out_protocol, service}).

init({Server, ProtoGen, Service, Handler}) when is_function(ProtoGen, 0) ->
    {ok, IProt, OProt} = ProtoGen(),
    loop(#thrift_processor{in_protocol = IProt,
                           out_protocol = OProt,
                           service = Service,
                           handler = Handler}).

loop(State = #thrift_processor{in_protocol  = IProto,
                               out_protocol = OProto}) ->
    case thrift_protocol:read(IProto, message_begin) of
        #protocol_message_begin{name = Function,
                                type = ?tMessageType_CALL} ->
            ok = handle_function(State, list_to_atom(Function)),
            loop(State);
        #protocol_message_begin{name = Function,
                                type = ?tMessageType_ONEWAY} ->
            ok = handle_function(State, list_to_atom(Function)),
            loop(State);
        {error, timeout} ->
            thrift_protocol:close_transport(OProto),
            ok;
        {error, closed} ->
            %% error_logger:info_msg("Client disconnected~n"),
            thrift_protocol:close_transport(OProto),
            exit(shutdown)
    end.

handle_function(State=#thrift_processor{in_protocol = IProto,
                                        out_protocol = OProto,
                                        handler = Handler,
                                        service = Service},
                Function) ->
    InParams = Service:function_info(Function, params_type),

    {ok, Params} = thrift_protocol:read(IProto, InParams),

    try
        Result = Handler:handle_function(Function, Params),
        %% {Micro, Result} = better_timer(Handler, handle_function, [Function, Params]),
        %% error_logger:info_msg("Processed ~p(~p) in ~.4fms~n",
        %%                       [Function, Params, Micro/1000.0]),
        handle_success(State, Function, Result)
    catch
        Type:Data ->
            handle_function_catch(State, Function, Type, Data)
    end,
    after_reply(OProto).

handle_function_catch(State = #thrift_processor{service = Service},
                      Function, ErrType, ErrData) ->
    IsOneway = Service:function_info(Function, reply_type) =:= oneway_void,

    case {ErrType, ErrData} of
        _ when IsOneway ->
            Stack = erlang:get_stacktrace(),
            error_logger:warning_msg(
              "oneway void ~p threw error which must be ignored: ~p",
              [Function, {ErrType, ErrData, Stack}]),
            ok;

        {throw, Exception} when is_tuple(Exception), size(Exception) > 0 ->
            error_logger:warning_msg("~p threw exception: ~p~n", [Function, Exception]),
            handle_exception(State, Function, Exception),
            ok;   % we still want to accept more requests from this client

        {error, Error} ->
            ok = handle_error(State, Function, Error)
    end.

handle_success(State = #thrift_processor{out_protocol = OProto,
                                         service = Service},
               Function,
               Result) ->
    ReplyType  = Service:function_info(Function, reply_type),
    StructName = atom_to_list(Function) ++ "_result",

    ok = case Result of
             {reply, ReplyData} ->
                 Reply = {{struct, [{0, ReplyType}]}, {StructName, ReplyData}},
                 send_reply(OProto, Function, ?tMessageType_REPLY, Reply);

             ok when ReplyType == {struct, []} ->
                 send_reply(OProto, Function, ?tMessageType_REPLY, {ReplyType, {StructName}});

             ok when ReplyType == oneway_void ->
                 %% no reply for oneway void
                 ok
         end.

handle_exception(State = #thrift_processor{out_protocol = OProto,
                                           service = Service},
                 Function,
                 Exception) ->
    ExceptionType = element(1, Exception),
    %% Fetch a structure like {struct, [{-2, {struct, {Module, Type}}},
    %%                                  {-3, {struct, {Module, Type}}}]}

    ReplySpec = Service:function_info(Function, exceptions),
    {struct, XInfo} = ReplySpec,

    true = is_list(XInfo),

    %% Assuming we had a type1 exception, we'd get: [undefined, Exception, undefined]
    %% e.g.: [{-1, type0}, {-2, type1}, {-3, type2}]
    ExceptionList = [case Type of
                         ExceptionType -> Exception;
                         _ -> undefined
                     end
                     || {_Fid, {struct, {_Module, Type}}} <- XInfo],

    ExceptionTuple = list_to_tuple([Function | ExceptionList]),

                                                % Make sure we got at least one defined
    case lists:all(fun(X) -> X =:= undefined end, ExceptionList) of
        true ->
            ok = handle_unknown_exception(State, Function, Exception);
        false ->
            ok = send_reply(OProto, Function, ?tMessageType_REPLY, {ReplySpec, ExceptionTuple})
    end.

%%
%% Called when an exception has been explicitly thrown by the service, but it was
%% not one of the exceptions that was defined for the function.
%%
handle_unknown_exception(State, Function, Exception) ->
    handle_error(State, Function, {exception_not_declared_as_thrown,
                                   Exception}).

handle_error(#thrift_processor{out_protocol = OProto}, Function, Error) ->
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

after_reply(OProto) ->
    ok = thrift_protocol:flush_transport(OProto)
    %%     ok = thrift_protocol:close_transport(OProto)
    .
