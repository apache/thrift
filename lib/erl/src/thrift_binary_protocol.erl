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

-module(thrift_binary_protocol).

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

-record(binary_protocol, {
    transport :: term(),
    strict_read = true :: boolean(),
    strict_write = true :: boolean(),
    % the most a message may take; new/2 settles it
    max_message_size :: pos_integer() | undefined,
    % what the message being read may still take; undefined between messages
    message_bytes_left :: non_neg_integer() | undefined
}).

-define(VERSION_MASK, 16#FFFF0000).
-define(VERSION_1, 16#80010000).
-define(TYPE_MASK, 16#000000ff).

new(Transport) ->
    new(Transport, _Options = []).

%%--------------------------------------------------------------------
%% Options include:
%%   {strict_read, Bool}, {strict_write, Bool}
%%   {max_message_size, Bytes}  = The longest message to read, in place of the
%%                                thrift application's max_message_size
%%--------------------------------------------------------------------
new(Transport, Options) ->
    State = #binary_protocol{transport = Transport},
    State1 = parse_options(Options, State),
    thrift_protocol:new(?MODULE, State1#binary_protocol{max_message_size = max_message_size(State1)}).

max_message_size(#binary_protocol{max_message_size = undefined}) ->
    application:get_env(thrift, max_message_size, ?DEFAULT_MAX_MESSAGE_SIZE);
max_message_size(#binary_protocol{max_message_size = Max}) ->
    Max.

parse_options([], State) ->
    State;
parse_options([{max_message_size, Max} | Rest], State) when is_integer(Max), Max > 0 ->
    parse_options(Rest, State#binary_protocol{max_message_size = Max});
parse_options([{strict_read, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_options(Rest, State#binary_protocol{strict_read = Bool});
parse_options([{strict_write, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_options(Rest, State#binary_protocol{strict_write = Bool}).

flush_transport(This = #binary_protocol{transport = Transport}) ->
    {NewTransport, Result} = thrift_transport:flush(Transport),
    {This#binary_protocol{transport = NewTransport}, Result}.

close_transport(This = #binary_protocol{transport = Transport}) ->
    {NewTransport, Result} = thrift_transport:close(Transport),
    {This#binary_protocol{transport = NewTransport}, Result}.

%%%
%%% instance methods
%%%

write(This0, #protocol_message_begin{
    name = Name,
    type = Type,
    seqid = Seqid
}) ->
    case This0#binary_protocol.strict_write of
        true ->
            {This1, ok} = write(This0, {i32, ?VERSION_1 bor Type}),
            {This2, ok} = write(This1, {string, Name}),
            {This3, ok} = write(This2, {i32, Seqid}),
            {This3, ok};
        false ->
            {This1, ok} = write(This0, {string, Name}),
            {This2, ok} = write(This1, {byte, Type}),
            {This3, ok} = write(This2, {i32, Seqid}),
            {This3, ok}
    end;
write(This, message_end) ->
    {This, ok};
write(This0, #protocol_field_begin{
    name = _Name,
    type = Type,
    id = Id
}) ->
    {This1, ok} = write(This0, {byte, Type}),
    {This2, ok} = write(This1, {i16, Id}),
    {This2, ok};
write(This, field_stop) ->
    write(This, {byte, ?tType_STOP});
write(This, field_end) ->
    {This, ok};
write(This0, #protocol_map_begin{
    ktype = Ktype,
    vtype = Vtype,
    size = Size
}) ->
    {This1, ok} = write(This0, {byte, Ktype}),
    {This2, ok} = write(This1, {byte, Vtype}),
    {This3, ok} = write(This2, {i32, Size}),
    {This3, ok};
write(This, map_end) ->
    {This, ok};
write(This0, #protocol_list_begin{
    etype = Etype,
    size = Size
}) ->
    {This1, ok} = write(This0, {byte, Etype}),
    {This2, ok} = write(This1, {i32, Size}),
    {This2, ok};
write(This, list_end) ->
    {This, ok};
write(This0, #protocol_set_begin{
    etype = Etype,
    size = Size
}) ->
    {This1, ok} = write(This0, {byte, Etype}),
    {This2, ok} = write(This1, {i32, Size}),
    {This2, ok};
write(This, set_end) ->
    {This, ok};
write(This, #protocol_struct_begin{}) ->
    {This, ok};
write(This, struct_end) ->
    {This, ok};
write(This, {bool, true}) ->
    write(This, {byte, 1});
write(This, {bool, false}) ->
    write(This, {byte, 0});
write(This, {byte, Byte}) ->
    write(This, <<Byte:8/big-signed>>);
write(This, {i16, I16}) ->
    write(This, <<I16:16/big-signed>>);
write(This, {i32, I32}) ->
    write(This, <<I32:32/big-signed>>);
write(This, {i64, I64}) ->
    write(This, <<I64:64/big-signed>>);
write(This, {double, Double}) ->
    write(This, <<Double:64/big-signed-float>>);
write(This0, {string, Str}) when is_list(Str) ->
    {This1, ok} = write(This0, {i32, length(Str)}),
    {This2, ok} = write(This1, list_to_binary(Str)),
    {This2, ok};
write(This0, {string, Bin}) when is_binary(Bin) ->
    {This1, ok} = write(This0, {i32, size(Bin)}),
    {This2, ok} = write(This1, Bin),
    {This2, ok};
%% Data :: iolist()
write(This = #binary_protocol{transport = Trans}, Data) ->
    {NewTransport, Result} = thrift_transport:write(Trans, Data),
    {This#binary_protocol{transport = NewTransport}, Result}.

%%

read(This0 = #binary_protocol{max_message_size = Max}, message_begin) ->
    {This1, Initial} = read(This0#binary_protocol{message_bytes_left = Max}, ui32),
    case Initial of
        {ok, Sz} when Sz band ?VERSION_MASK =:= ?VERSION_1 ->
            %% we're at version 1
            case read(This1, string) of
                {This2, {ok, Name}} ->
                    read_message_begin(This2, Name, Sz band ?TYPE_MASK);
                {This2, Error} ->
                    {This2, Error}
            end;
        {ok, Sz} when Sz < 0 ->
            %% there's a version number but it's unexpected
            {This1, {error, {bad_binary_protocol_version, Sz}}};
        {ok, _Sz} when This1#binary_protocol.strict_read =:= true ->
            %% strict_read is true and there's no version header; that's an error
            {This1, {error, no_binary_protocol_version}};
        {ok, Sz} when This1#binary_protocol.strict_read =:= false ->
            %% strict_read is false, so just read the old way
            case read_data(This1, Sz) of
                {This2, {ok, Name}} ->
                    case read(This2, byte) of
                        {This3, {ok, Type}} -> read_message_begin(This3, Name, Type);
                        {This3, Error} -> {This3, Error}
                    end;
                {This2, Error} ->
                    {This2, Error}
            end;
        Else ->
            {This1, Else}
    end;
read(This, message_end) ->
    {This#binary_protocol{message_bytes_left = undefined}, ok};
read(This, struct_begin) ->
    {This, ok};
read(This, struct_end) ->
    {This, ok};
read(This0, field_begin) ->
    {This1, Result} = read(This0, byte),
    case Result of
        {ok, Type = ?tType_STOP} ->
            {This1, #protocol_field_begin{type = Type}};
        {ok, Type} ->
            {This2, {ok, Id}} = read(This1, i16),
            {This2, #protocol_field_begin{
                type = Type,
                id = Id
            }}
    end;
read(This, field_end) ->
    {This, ok};
read(This0, map_begin) ->
    {This1, {ok, Ktype}} = read(This0, byte),
    {This2, {ok, Vtype}} = read(This1, byte),
    {This3, {ok, Size}} = read(This2, i32),
    if Size < 0 -> error({protocol_error, negative_size}); true -> ok end,
    {This3, #protocol_map_begin{
        ktype = Ktype,
        vtype = Vtype,
        size = Size
    }};
read(This, map_end) ->
    {This, ok};
read(This0, list_begin) ->
    {This1, {ok, Etype}} = read(This0, byte),
    {This2, {ok, Size}} = read(This1, i32),
    if Size < 0 -> error({protocol_error, negative_size}); true -> ok end,
    {This2, #protocol_list_begin{
        etype = Etype,
        size = Size
    }};
read(This, list_end) ->
    {This, ok};
read(This0, set_begin) ->
    {This1, {ok, Etype}} = read(This0, byte),
    {This2, {ok, Size}} = read(This1, i32),
    if Size < 0 -> error({protocol_error, negative_size}); true -> ok end,
    {This2, #protocol_set_begin{
        etype = Etype,
        size = Size
    }};
read(This, set_end) ->
    {This, ok};
read(This0, field_stop) ->
    {This1, {ok, ?tType_STOP}} = read(This0, byte),
    {This1, ok};
%%

read(This0, bool) ->
    {This1, Result} = read(This0, byte),
    case Result of
        {ok, Byte} -> {This1, {ok, Byte /= 0}};
        Else -> {This1, Else}
    end;
read(This0, byte) ->
    {This1, Bytes} = read_data(This0, 1),
    case Bytes of
        {ok, <<Val:8/integer-signed-big, _/binary>>} -> {This1, {ok, Val}};
        Else -> {This1, Else}
    end;
read(This0, i16) ->
    {This1, Bytes} = read_data(This0, 2),
    case Bytes of
        {ok, <<Val:16/integer-signed-big, _/binary>>} -> {This1, {ok, Val}};
        Else -> {This1, Else}
    end;
read(This0, i32) ->
    {This1, Bytes} = read_data(This0, 4),
    case Bytes of
        {ok, <<Val:32/integer-signed-big, _/binary>>} -> {This1, {ok, Val}};
        Else -> {This1, Else}
    end;
%% unsigned ints aren't used by thrift itself, but it's used for the parsing
%% of the packet version header. Without this special function BEAM works fine
%% but hipe thinks it received a bad version header.
read(This0, ui32) ->
    {This1, Bytes} = read_data(This0, 4),
    case Bytes of
        {ok, <<Val:32/integer-unsigned-big, _/binary>>} -> {This1, {ok, Val}};
        Else -> {This1, Else}
    end;
read(This0, i64) ->
    {This1, Bytes} = read_data(This0, 8),
    case Bytes of
        {ok, <<Val:64/integer-signed-big, _/binary>>} -> {This1, {ok, Val}};
        Else -> {This1, Else}
    end;
read(This0, double) ->
    {This1, Bytes} = read_data(This0, 8),
    case Bytes of
        {ok, <<Val:64/float-signed-big, _/binary>>} -> {This1, {ok, Val}};
        Else -> {This1, Else}
    end;
% returns a binary directly, call binary_to_list if necessary
read(This0, string) ->
    {This1, {ok, Sz}} = read(This0, i32),
    if Sz < 0 -> error({protocol_error, negative_size}); true -> ok end,
    read_data(This1, Sz).

%% The sequence id that ends a message header, once its name and type are read.
read_message_begin(This0, Name, Type) ->
    case read(This0, i32) of
        {This1, {ok, SeqId}} ->
            {This1, #protocol_message_begin{
                name = binary_to_list(Name),
                type = Type,
                seqid = SeqId
            }};
        {This1, Error} ->
            {This1, Error}
    end.

-spec read_data(#binary_protocol{}, non_neg_integer()) ->
    {#binary_protocol{}, {ok, binary()} | {error, _Reason}}.
read_data(This, 0) ->
    {This, {ok, <<>>}};
read_data(This0 = #binary_protocol{transport = Trans}, Len) when is_integer(Len) andalso Len > 0 ->
    case take(This0, Len) of
        {ok, This1} ->
            {NewTransport, Result} = thrift_transport:read(Trans, Len),
            {This1#binary_protocol{transport = NewTransport}, Result};
        Error ->
            {This0, Error}
    end.

%% Takes Len bytes from what the message being read may still take, before
%% they are read. Between messages, each read is held to the maximum on its own.
take(This = #binary_protocol{message_bytes_left = undefined, max_message_size = Max}, Len) when
    Len =< Max
->
    {ok, This};
take(This = #binary_protocol{message_bytes_left = Left}, Len) when is_integer(Left), Len =< Left ->
    {ok, This#binary_protocol{message_bytes_left = Left - Len}};
take(#binary_protocol{max_message_size = Max}, _Len) ->
    {error, {message_size_exceeds_maximum, Max}}.

%%%% FACTORY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(tbp_opts, {
    strict_read = true :: boolean(),
    strict_write = true :: boolean(),
    max_message_size :: pos_integer() | undefined
}).

parse_factory_options([], Opts) ->
    Opts;
parse_factory_options([{max_message_size, Max} | Rest], Opts) when is_integer(Max), Max > 0 ->
    parse_factory_options(Rest, Opts#tbp_opts{max_message_size = Max});
parse_factory_options([{strict_read, Bool} | Rest], Opts) when is_boolean(Bool) ->
    parse_factory_options(Rest, Opts#tbp_opts{strict_read = Bool});
parse_factory_options([{strict_write, Bool} | Rest], Opts) when is_boolean(Bool) ->
    parse_factory_options(Rest, Opts#tbp_opts{strict_write = Bool}).

%% returns a (fun() -> thrift_protocol())
new_protocol_factory(TransportFactory, Options) ->
    ParsedOpts = parse_factory_options(Options, #tbp_opts{}),
    F = fun() ->
        case TransportFactory() of
            {ok, Transport} ->
                thrift_binary_protocol:new(
                    Transport,
                    [
                        {strict_read, ParsedOpts#tbp_opts.strict_read},
                        {strict_write, ParsedOpts#tbp_opts.strict_write}
                        | [
                            {max_message_size, Max}
                         || Max <- [ParsedOpts#tbp_opts.max_message_size], Max =/= undefined
                        ]
                    ]
                );
            {error, Error} ->
                {error, Error}
        end
    end,
    {ok, F}.
