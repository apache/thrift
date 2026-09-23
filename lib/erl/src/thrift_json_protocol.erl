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
%% The JSON protocol implementation was created by
%% Peter Neumark <neumark.peter@gmail.com> based on
%% the binary protocol implementation.

-module(thrift_json_protocol).

-behaviour(thrift_protocol).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-export([
    new/1, new/2,
    read/2,
    write/2,
    flush_transport/1,
    close_transport/1,
    new_protocol_factory/2
]).

-record(json_context, {
    % the type of json_context: array or object
    type :: undefined | array | object,
    % fields read or written
    fields_processed = 0 :: non_neg_integer()
}).

-type json_context() :: #json_context{}.
-type jsx_type() :: atom() | {atom(), atom() | binary() | number()}.
-type jsx() :: {event, jsx_type(), [jsx_type()]}.

-record(json_protocol, {
    transport :: term(),
    context_stack = [] :: [json_context()],
    jsx :: undefined | jsx(),
    % undefined: the thrift application's max_message_size
    max_message_size :: pos_integer() | undefined
}).

-define(VERSION_1, 1).
-define(JSON_DOUBLE_PRECISION, 16).

typeid_to_json(?tType_BOOL) -> "tf";
typeid_to_json(?tType_DOUBLE) -> "dbl";
% NOTE: ?tType_BYTE also match here
typeid_to_json(?tType_I8) -> "i8";
typeid_to_json(?tType_I16) -> "i16";
typeid_to_json(?tType_I32) -> "i32";
typeid_to_json(?tType_I64) -> "i64";
typeid_to_json(?tType_STRING) -> "str";
typeid_to_json(?tType_STRUCT) -> "rec";
typeid_to_json(?tType_MAP) -> "map";
typeid_to_json(?tType_SET) -> "set";
typeid_to_json(?tType_LIST) -> "lst".

json_to_typeid(<<"tf">>) -> ?tType_BOOL;
json_to_typeid(<<"dbl">>) -> ?tType_DOUBLE;
json_to_typeid(<<"i8">>) -> ?tType_I8;
json_to_typeid(<<"i16">>) -> ?tType_I16;
json_to_typeid(<<"i32">>) -> ?tType_I32;
json_to_typeid(<<"i64">>) -> ?tType_I64;
json_to_typeid(<<"str">>) -> ?tType_STRING;
json_to_typeid(<<"rec">>) -> ?tType_STRUCT;
json_to_typeid(<<"map">>) -> ?tType_MAP;
json_to_typeid(<<"set">>) -> ?tType_SET;
json_to_typeid(<<"lst">>) -> ?tType_LIST.

start_context(object) -> "{";
start_context(array) -> "[".

end_context(object) -> "}";
end_context(array) -> "]".

new(Transport) ->
    new(Transport, _Options = []).

%%--------------------------------------------------------------------
%% Options include:
%%   {max_message_size, Bytes}  = The longest message to read, in place of the
%%                                thrift application's max_message_size
%%--------------------------------------------------------------------
new(Transport, Options) when is_list(Options) ->
    State = lists:foldl(fun apply_option/2, #json_protocol{transport = Transport}, Options),
    thrift_protocol:new(?MODULE, State);
new(Transport, _Options) ->
    new(Transport, []).

apply_option({max_message_size, Max}, State) when is_integer(Max), Max > 0 ->
    State#json_protocol{max_message_size = Max};
apply_option(_Other, State) ->
    State.

max_message_size(#json_protocol{max_message_size = undefined}) ->
    application:get_env(thrift, max_message_size, ?DEFAULT_MAX_MESSAGE_SIZE);
max_message_size(#json_protocol{max_message_size = Max}) ->
    Max.

flush_transport(This = #json_protocol{transport = Transport}) ->
    {NewTransport, Result} = thrift_transport:flush(Transport),
    {
        This#json_protocol{
            transport = NewTransport,
            context_stack = []
        },
        Result
    }.

close_transport(This = #json_protocol{transport = Transport}) ->
    {NewTransport, Result} = thrift_transport:close(Transport),
    {
        This#json_protocol{
            transport = NewTransport,
            context_stack = [],
            jsx = undefined
        },
        Result
    }.

%%%
%%% instance methods
%%%
% places a new context on the stack:
write(#json_protocol{context_stack = Stack} = State0, {enter_context, Type}) ->
    {State1, ok} = write_values(State0, [{context_pre_item, false}]),
    State2 = State1#json_protocol{
        context_stack = [
            #json_context{type = Type} | Stack
        ]
    },
    write_values(State2, [list_to_binary(start_context(Type))]);
% removes the topmost context from stack
write(#json_protocol{context_stack = [CurrCtxt | Stack]} = State0, {exit_context}) ->
    Type = CurrCtxt#json_context.type,
    State1 = State0#json_protocol{context_stack = Stack},
    write_values(State1, [
        list_to_binary(end_context(Type)),
        {context_post_item, false}
    ]);
% writes necessary prelude to field or container depending on current context
write(
    #json_protocol{context_stack = []} = This0,
    {context_pre_item, _}
) ->
    {This0, ok};
write(
    #json_protocol{context_stack = [Context | _CtxtTail]} = This0,
    {context_pre_item, MayNeedQuotes}
) ->
    FieldNo = Context#json_context.fields_processed,
    CtxtType = Context#json_context.type,
    Rem = FieldNo rem 2,
    case {CtxtType, FieldNo, Rem, MayNeedQuotes} of
        % array element (not first)
        {array, N, _, _} when N > 0 ->
            write(This0, <<",">>);
        % non-string object key (first)
        {object, 0, _, true} ->
            write(This0, <<"\"">>);
        % non-string object key (not first)
        {object, N, 0, true} when N > 0 ->
            write(This0, <<",\"">>);
        % string object key (not first)
        {object, N, 0, false} when N > 0 ->
            write(This0, <<",">>);
        % no pre-field necessary
        _ ->
            {This0, ok}
    end;
% writes necessary postlude to field or container depending on current context
write(
    #json_protocol{context_stack = []} = This0,
    {context_post_item, _}
) ->
    {This0, ok};
write(
    #json_protocol{context_stack = [Context | CtxtTail]} = This0,
    {context_post_item, MayNeedQuotes}
) ->
    FieldNo = Context#json_context.fields_processed,
    CtxtType = Context#json_context.type,
    Rem = FieldNo rem 2,
    {This1, ok} =
        case {CtxtType, Rem, MayNeedQuotes} of
            % non-string object key
            {object, 0, true} ->
                write(This0, <<"\":">>);
            % string object key
            {object, 0, false} ->
                write(This0, <<":">>);
            % no pre-field necessary
            _ ->
                {This0, ok}
        end,
    NewContext = Context#json_context{fields_processed = FieldNo + 1},
    {This1#json_protocol{context_stack = [NewContext | CtxtTail]}, ok};
write(This0, #protocol_message_begin{
    name = Name,
    type = Type,
    seqid = Seqid
}) ->
    write_values(This0, [
        {enter_context, array},
        {i32, ?VERSION_1},
        {string, Name},
        {i32, Type},
        {i32, Seqid}
    ]);
write(This, message_end) ->
    write_values(This, [{exit_context}]);
% Example field expression: "1":{"dbl":3.14}
write(This0, #protocol_field_begin{
    name = _Name,
    type = Type,
    id = Id
}) ->
    write_values(This0, [
        % entering 'outer' object
        {i16, Id},
        % entering 'outer' object
        {enter_context, object},
        {string, typeid_to_json(Type)}
    ]);
write(This, field_stop) ->
    {This, ok};
write(This, field_end) ->
    write_values(This, [{exit_context}]);
% Example message with map: [1,"testMap",1,0,{"1":{"map":["i32","i32",3,{"7":77,"8":88,"9":99}]}}]
write(This0, #protocol_map_begin{
    ktype = Ktype,
    vtype = Vtype,
    size = Size
}) ->
    write_values(This0, [
        {enter_context, array},
        {string, typeid_to_json(Ktype)},
        {string, typeid_to_json(Vtype)},
        {i32, Size},
        {enter_context, object}
    ]);
write(This, map_end) ->
    write_values(This, [
        {exit_context},
        {exit_context}
    ]);
write(This0, #protocol_list_begin{
    etype = Etype,
    size = Size
}) ->
    write_values(This0, [
        {enter_context, array},
        {string, typeid_to_json(Etype)},
        {i32, Size}
    ]);
write(This, list_end) ->
    write_values(This, [
        {exit_context}
    ]);
% example message with set: [1,"testSet",1,0,{"1":{"set":["i32",3,1,2,3]}}]
write(This0, #protocol_set_begin{
    etype = Etype,
    size = Size
}) ->
    write_values(This0, [
        {enter_context, array},
        {string, typeid_to_json(Etype)},
        {i32, Size}
    ]);
write(This, set_end) ->
    write_values(This, [
        {exit_context}
    ]);
% example message with struct: [1,"testStruct",1,0,{"1":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":1152921504606847000}}}}]
write(This, #protocol_struct_begin{}) ->
    write_values(This, [
        {enter_context, object}
    ]);
write(This, struct_end) ->
    write_values(This, [
        {exit_context}
    ]);
write(This, {bool, true}) ->
    write_values(This, [
        {context_pre_item, true},
        <<"true">>,
        {context_post_item, true}
    ]);
write(This, {bool, false}) ->
    write_values(This, [
        {context_pre_item, true},
        <<"false">>,
        {context_post_item, true}
    ]);
write(This, {byte, Byte}) ->
    write_values(This, [
        {context_pre_item, true},
        list_to_binary(integer_to_list(Byte)),
        {context_post_item, true}
    ]);
write(This, {i16, I16}) ->
    write(This, {byte, I16});
write(This, {i32, I32}) ->
    write(This, {byte, I32});
write(This, {i64, I64}) ->
    write(This, {byte, I64});
write(This, {double, Double}) ->
    write_values(This, [
        {context_pre_item, true},
        list_to_binary(io_lib:format("~.*f", [?JSON_DOUBLE_PRECISION, Double])),
        {context_post_item, true}
    ]);
write(This0, {string, Str}) ->
    write_values(This0, [
        {context_pre_item, false},
        <<"\"", (escape(iolist_to_binary(Str)))/binary, "\"">>,
        {context_post_item, false}
    ]);
%% TODO: binary fields should be base64 encoded?

%% Data :: iolist()
write(This = #json_protocol{transport = Trans}, Data) ->
    %io:format("Data ~p Ctxt ~p~n~n", [Data, This#json_protocol.context_stack]),
    {NewTransport, Result} = thrift_transport:write(Trans, Data),
    {This#json_protocol{transport = NewTransport}, Result}.

write_values(This0, ValueList) ->
    FinalState = lists:foldl(
        fun(Val, ThisIn) ->
            {ThisOut, ok} = write(ThisIn, Val),
            ThisOut
        end,
        This0,
        ValueList
    ),
    {FinalState, ok}.

%% JSON requires the quote, the backslash and the control characters to be
%% escaped in a string. Everything else, UTF-8 included, goes as it is.
escape(Bin) ->
    <<<<(escape_byte(B))/binary>> || <<B>> <= Bin>>.

escape_byte($") -> <<"\\\"">>;
escape_byte($\\) -> <<"\\\\">>;
escape_byte($\b) -> <<"\\b">>;
escape_byte($\f) -> <<"\\f">>;
escape_byte($\n) -> <<"\\n">>;
escape_byte($\r) -> <<"\\r">>;
escape_byte($\t) -> <<"\\t">>;
escape_byte(B) when B < 16#20 -> iolist_to_binary(io_lib:format("\\u~4.16.0b", [B]));
escape_byte(B) -> <<B>>.

%% Reads a whole message and hands it to the JSON parser. Subsequent calls to
%% read operate on the events the parser returned.
%%
%% The message ends where its outer array closes, which scanning the bytes
%% for brackets outside of strings finds. The transport is asked each time
%% for no more bytes than the message still needs at least. A socket
%% transport, and a buffered one over it, waits until it has all the bytes
%% it was asked for, and the stream goes on with the next message; other
%% transports, framed among them, return what they have and keep the rest.
%%
%% A message longer than max_message_size is refused before more than that
%% many bytes of it are read.
read_all(#json_protocol{transport = Transport0} = State) ->
    case read_message(Transport0, max_message_size(State), [], 0, 0, false, false) of
        {Transport1, {ok, Bin}} ->
            P = thrift_json_parser:parser(),
            [First | Rest] = P(Bin),
            {State#json_protocol{transport = Transport1, jsx = {event, First, Rest}}, ok};
        {Transport1, {error, _} = Error} ->
            {State#json_protocol{transport = Transport1}, Error}
    end.

read_message(Transport0, Max, Parts, Size, Depth, InString, Escaped) ->
    Least = least_left(Depth, InString, Escaped),
    case Size + Least > Max of
        true ->
            {Transport0, {error, {message_size_exceeds_maximum, Max}}};
        false ->
            {Transport1, Result} = thrift_transport:read(Transport0, Least),
            case Result of
                {ok, <<>>} ->
                    {Transport1, {error, eof}};
                {ok, Data} ->
                    case scan(Data, Depth, InString, Escaped) of
                        done ->
                            {Transport1, {ok, iolist_to_binary(lists:reverse([Data | Parts]))}};
                        {Depth1, InString1, Escaped1} ->
                            read_message(
                                Transport1,
                                Max,
                                [Data | Parts],
                                Size + byte_size(Data),
                                Depth1,
                                InString1,
                                Escaped1
                            )
                    end;
                {error, _} = Error ->
                    {Transport1, Error}
            end
    end.

%% The fewest bytes that can still complete the message: a closing bracket
%% for each open one, and the end of an open string, with the character an
%% escape in it still needs.
least_left(0, _, _) -> 1;
least_left(Depth, false, _) -> Depth;
least_left(Depth, true, false) -> Depth + 1;
least_left(Depth, true, true) -> Depth + 2.

%% Follows the brackets of a message through Data: done once the outer one
%% has closed, or else where the message has got to. A message that does not
%% start with a bracket is done at once, and the parser reports it.
scan(<<>>, Depth, InString, Escaped) ->
    {Depth, InString, Escaped};
scan(<<C, Rest/binary>>, 0, _, _) ->
    if
        C =:= $[; C =:= ${ -> scan(Rest, 1, false, false);
        C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n -> scan(Rest, 0, false, false);
        true -> done
    end;
scan(<<_, Rest/binary>>, Depth, true, true) ->
    scan(Rest, Depth, true, false);
scan(<<$\\, Rest/binary>>, Depth, true, false) ->
    scan(Rest, Depth, true, true);
scan(<<$", Rest/binary>>, Depth, InString, false) ->
    scan(Rest, Depth, not InString, false);
scan(<<_, Rest/binary>>, Depth, true, false) ->
    scan(Rest, Depth, true, false);
scan(<<C, Rest/binary>>, Depth, false, false) when C =:= $[; C =:= ${ ->
    scan(Rest, Depth + 1, false, false);
scan(<<C, _/binary>>, 1, false, false) when C =:= $]; C =:= $} ->
    done;
scan(<<C, Rest/binary>>, Depth, false, false) when C =:= $]; C =:= $} ->
    scan(Rest, Depth - 1, false, false);
scan(<<_, Rest/binary>>, Depth, false, false) ->
    scan(Rest, Depth, false, false).

% Expect reads an event from the JSX event stream. It receives an event or data
% type as input. Comparing the read event from the one is was passed, it
% returns an error if something other than the expected value is encountered.
% Expect also maintains the context stack in #json_protocol.
expect(#json_protocol{jsx = {event, {Type, Data} = Ev, [Next | Rest]}} = State, ExpectedType) ->
    NextState = State#json_protocol{jsx = {event, Next, Rest}},
    case Type == ExpectedType of
        true ->
            %% The parser hands over numbers as numbers, and keys and
            %% strings as binaries.
            {NextState, {ok, Data}};
        false ->
            {NextState, {error, {unexpected_json_event, Ev}}}
    end;
expect(#json_protocol{jsx = {event, Event, Next}} = State, ExpectedEvent) ->
    expect(State#json_protocol{jsx = {event, {Event, none}, Next}}, ExpectedEvent).

%% The next event, left for the next read to take.
peek(#json_protocol{jsx = {event, Event, _}}) ->
    Event.

expect_many(State, ExpectedList) ->
    expect_many_1(State, ExpectedList, [], ok).

expect_many_1(State, [], ResultList, Status) ->
    {State, {Status, lists:reverse(ResultList)}};
expect_many_1(State, [Expected | ExpTail], ResultList, _PrevStatus) ->
    {State1, {Status, Data}} = expect(State, Expected),
    NewResultList = [Data | ResultList],
    case Status of
        % in case of error, end prematurely
        error -> expect_many_1(State1, [], NewResultList, Status);
        ok -> expect_many_1(State1, ExpTail, NewResultList, Status)
    end.

% wrapper around expect to make life easier for container opening/closing functions
expect_nodata(This, ExpectedList) ->
    case expect_many(This, ExpectedList) of
        {State, {ok, _}} ->
            {State, ok};
        Error ->
            Error
    end.

read_field(#json_protocol{jsx = {event, Field, [Next | Rest]}} = State) ->
    NewState = State#json_protocol{jsx = {event, Next, Rest}},
    {NewState, Field}.

read(This0, message_begin) ->
    case read_all(This0) of
        {This1, ok} ->
            read_message_begin(This1);
        {This1, {error, _} = Error} ->
            {This1, Error}
    end;
read(This, message_end) ->
    expect_nodata(This, [end_array]);
read(This, struct_begin) ->
    expect_nodata(This, [start_object]);
read(This, struct_end) ->
    expect_nodata(This, [end_object]);
%% The end of the struct is the stop field. Its end_object is left for
%% struct_end, which is read next.
read(This0, field_begin) ->
    case peek(This0) of
        end_object ->
            {This0, #protocol_field_begin{type = ?tType_STOP}};
        _ ->
            {This1, Read} = expect_many(
                This0,
                %field id
                [
                    key,
                    % {} surrounding field
                    start_object,
                    % type of field
                    key
                ]
            ),
            case Read of
                {ok, [FieldId, _, FieldType]} ->
                    {This1, #protocol_field_begin{
                        type = json_to_typeid(FieldType),
                        id = binary_to_integer(FieldId)
                    }};
                Other ->
                    io:format("**** OTHER branch selected ****"),
                    {This1, Other}
            end
    end;
read(This, field_end) ->
    expect_nodata(This, [end_object]);
% Example message with map: [1,"testMap",1,0,{"1":{"map":["i32","i32",3,{"7":77,"8":88,"9":99}]}}]
read(This0, map_begin) ->
    case
        expect_many(
            This0,
            [
                start_array,
                % key type
                string,
                % value type
                string,
                % size
                integer,
                % the following object contains the map
                start_object
            ]
        )
    of
        {This1, {ok, [_, Ktype, Vtype, Size, _]}} ->
            {This1, #protocol_map_begin{
                ktype = json_to_typeid(Ktype),
                vtype = json_to_typeid(Vtype),
                size = Size
            }};
        Other ->
            Other
    end;
read(This, map_end) ->
    expect_nodata(This, [end_object, end_array]);
read(This0, list_begin) ->
    case
        expect_many(
            This0,
            [
                start_array,
                % element type
                string,
                % size
                integer
            ]
        )
    of
        {This1, {ok, [_, Etype, Size]}} ->
            {This1, #protocol_list_begin{
                etype = json_to_typeid(Etype),
                size = Size
            }};
        Other ->
            Other
    end;
read(This, list_end) ->
    expect_nodata(This, [end_array]);
% example message with set: [1,"testSet",1,0,{"1":{"set":["i32",3,1,2,3]}}]
read(This0, set_begin) ->
    case
        expect_many(
            This0,
            [
                start_array,
                % element type
                string,
                % size
                integer
            ]
        )
    of
        {This1, {ok, [_, Etype, Size]}} ->
            {This1, #protocol_set_begin{
                etype = json_to_typeid(Etype),
                size = Size
            }};
        Other ->
            Other
    end;
read(This, set_end) ->
    expect_nodata(This, [end_array]);
read(This0, field_stop) ->
    {This0, ok};
%%

%% A value that is the key of a map comes as the text of an object key.
read(This0, bool) ->
    {This1, Field} = read_field(This0),
    Value =
        case Field of
            {literal, B} when is_boolean(B) ->
                {ok, B};
            {key, <<"true">>} ->
                {ok, true};
            {key, <<"false">>} ->
                {ok, false};
            _Other ->
                {error, unexpected_event_for_boolean}
        end,
    {This1, Value};
read(This0, byte) ->
    {This1, Field} = read_field(This0),
    Value =
        case Field of
            {key, K} ->
                key_to_number(K, fun binary_to_integer/1, unexpected_event_for_integer);
            {integer, I} ->
                {ok, I};
            _Other ->
                {error, unexpected_event_for_integer}
        end,
    {This1, Value};
read(This0, i16) ->
    read(This0, byte);
read(This0, i32) ->
    read(This0, byte);
read(This0, i64) ->
    read(This0, byte);
read(This0, double) ->
    {This1, Field} = read_field(This0),
    Value =
        case Field of
            {key, K} ->
                key_to_number(K, fun binary_to_float/1, unexpected_event_for_double);
            {float, F} ->
                {ok, F};
            _Other ->
                {error, unexpected_event_for_double}
        end,
    {This1, Value};
% returns a binary directly, call binary_to_list if necessary
read(This0, string) ->
    {This1, Field} = read_field(This0),
    Value =
        case Field of
            {string, I} ->
                {ok, I};
            {key, J} ->
                {ok, J};
            _Other ->
                {error, unexpected_event_for_string}
        end,
    {This1, Value}.

read_message_begin(This0) ->
    case
        expect_many(
            This0,
            [start_array, integer, string, integer, integer]
        )
    of
        {This1, {ok, [_, Version, Name, Type, SeqId]}} ->
            case Version =:= ?VERSION_1 of
                true ->
                    {This1, #protocol_message_begin{
                        name = binary_to_list(Name),
                        type = Type,
                        seqid = SeqId
                    }};
                false ->
                    {This1, {error, no_json_protocol_version}}
            end;
        Other ->
            Other
    end.

key_to_number(Key, Convert, Error) ->
    try Convert(Key) of
        Number -> {ok, Number}
    catch
        error:badarg -> {error, Error}
    end.

%%%% FACTORY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% returns a (fun() -> thrift_protocol())
new_protocol_factory(TransportFactory, _Options) ->
    % Only strice read/write are implemented
    F = fun() ->
        {ok, Transport} = TransportFactory(),
        thrift_json_protocol:new(Transport, [])
    end,
    {ok, F}.
