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

-behavior(thrift_protocol).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-export([new/1, new/2,
         read/2,
         write/2,
         flush_transport/1,
         close_transport/1,

         new_protocol_factory/2
        ]).

-record(binary_protocol, {transport,
                          strict_read=true,
                          strict_write=true
                         }).

-define(VERSION_MASK, 16#FFFF0000).
-define(VERSION_1, 16#80010000).
-define(TYPE_MASK, 16#000000ff).

new(Transport) ->
    new(Transport, _Options = []).

new(Transport, Options) ->
    State  = #binary_protocol{transport = Transport},
    State1 = parse_options(Options, State),
    thrift_protocol:new(?MODULE, State1).

parse_options([], State) ->
    State;
parse_options([{strict_read, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_options(Rest, State#binary_protocol{strict_read=Bool});
parse_options([{strict_write, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_options(Rest, State#binary_protocol{strict_write=Bool}).


flush_transport(#binary_protocol{transport = Transport}) ->
    thrift_transport:flush(Transport).

close_transport(#binary_protocol{transport = Transport}) ->
    thrift_transport:close(Transport).

%%%
%%% instance methods
%%%

write(This, #protocol_message_begin{
        name = Name,
        type = Type,
        seqid = Seqid}) ->
    case This#binary_protocol.strict_write of
        true ->
            write(This, {i32, ?VERSION_1 bor Type}),
            write(This, {string, Name}),
            write(This, {i32, Seqid});
        false ->
            write(This, {string, Name}),
            write(This, {byte, Type}),
            write(This, {i32, Seqid})
    end,
    ok;

write(This, message_end) -> ok;

write(This, #protocol_field_begin{
       name = _Name,
       type = Type,
       id = Id}) ->
    write(This, {byte, Type}),
    write(This, {i16, Id}),
    ok;

write(This, field_stop) ->
    write(This, {byte, ?tType_STOP}),
    ok;

write(This, field_end) -> ok;

write(This, #protocol_map_begin{
       ktype = Ktype,
       vtype = Vtype,
       size = Size}) ->
    write(This, {byte, Ktype}),
    write(This, {byte, Vtype}),
    write(This, {i32, Size}),
    ok;

write(This, map_end) -> ok;

write(This, #protocol_list_begin{
        etype = Etype,
        size = Size}) ->
    write(This, {byte, Etype}),
    write(This, {i32, Size}),
    ok;

write(This, list_end) -> ok;

write(This, #protocol_set_begin{
        etype = Etype,
        size = Size}) ->
    write(This, {byte, Etype}),
    write(This, {i32, Size}),
    ok;

write(This, set_end) -> ok;

write(This, #protocol_struct_begin{}) -> ok;
write(This, struct_end) -> ok;

write(This, {bool, true})  -> write(This, {byte, 1});
write(This, {bool, false}) -> write(This, {byte, 0});

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

write(This, {string, Str}) when is_list(Str) ->
    write(This, {i32, length(Str)}),
    write(This, list_to_binary(Str));

write(This, {string, Bin}) when is_binary(Bin) ->
    write(This, {i32, size(Bin)}),
    write(This, Bin);

%% Data :: iolist()
write(This, Data) ->
    thrift_transport:write(This#binary_protocol.transport, Data).

%%

read(This, message_begin) ->
    case read(This, ui32) of
        {ok, Sz} when Sz band ?VERSION_MASK =:= ?VERSION_1 ->
            %% we're at version 1
            {ok, Name}  = read(This, string),
            Type        = Sz band ?TYPE_MASK,
            {ok, SeqId} = read(This, i32),
            #protocol_message_begin{name  = binary_to_list(Name),
                                    type  = Type,
                                    seqid = SeqId};

        {ok, Sz} when Sz < 0 ->
            %% there's a version number but it's unexpected
            {error, {bad_binary_protocol_version, Sz}};

        {ok, Sz} when This#binary_protocol.strict_read =:= true ->
            %% strict_read is true and there's no version header; that's an error
            {error, no_binary_protocol_version};

        {ok, Sz} when This#binary_protocol.strict_read =:= false ->
            %% strict_read is false, so just read the old way
            {ok, Name}  = read(This, Sz),
            {ok, Type}  = read(This, byte),
            {ok, SeqId} = read(This, i32),
            #protocol_message_begin{name  = binary_to_list(Name),
                                    type  = Type,
                                    seqid = SeqId};

        Err = {error, closed} -> Err;
        Err = {error, timeout}-> Err;
        Err = {error, ebadf}  -> Err
    end;

read(This, message_end) -> ok;

read(This, struct_begin) -> ok;
read(This, struct_end) -> ok;

read(This, field_begin) ->
    case read(This, byte) of
        {ok, Type = ?tType_STOP} ->
            #protocol_field_begin{type = Type};
        {ok, Type} ->
            {ok, Id} = read(This, i16),
            #protocol_field_begin{type = Type,
                                  id = Id}
    end;

read(This, field_end) -> ok;

read(This, map_begin) ->
    {ok, Ktype} = read(This, byte),
    {ok, Vtype} = read(This, byte),
    {ok, Size}  = read(This, i32),
    #protocol_map_begin{ktype = Ktype,
                        vtype = Vtype,
                        size = Size};
read(This, map_end) -> ok;

read(This, list_begin) ->
    {ok, Etype} = read(This, byte),
    {ok, Size}  = read(This, i32),
    #protocol_list_begin{etype = Etype,
                         size = Size};
read(This, list_end) -> ok;

read(This, set_begin) ->
    {ok, Etype} = read(This, byte),
    {ok, Size}  = read(This, i32),
    #protocol_set_begin{etype = Etype,
                        size = Size};
read(This, set_end) -> ok;

read(This, field_stop) ->
    {ok, ?tType_STOP} =  read(This, byte),
    ok;

%%

read(This, bool) ->
    case read(This, byte) of
        {ok, Byte} -> {ok, Byte /= 0};
        Else -> Else
    end;

read(This, byte) ->
    case read(This, 1) of
        {ok, <<Val:8/integer-signed-big, _/binary>>} -> {ok, Val};
        Else -> Else
    end;

read(This, i16) ->
    case read(This, 2) of
        {ok, <<Val:16/integer-signed-big, _/binary>>} -> {ok, Val};
        Else -> Else
    end;

read(This, i32) ->
    case read(This, 4) of
        {ok, <<Val:32/integer-signed-big, _/binary>>} -> {ok, Val};
        Else -> Else
    end;

%% unsigned ints aren't used by thrift itself, but it's used for the parsing
%% of the packet version header. Without this special function BEAM works fine
%% but hipe thinks it received a bad version header.
read(This, ui32) ->
    case read(This, 4) of
        {ok, <<Val:32/integer-unsigned-big, _/binary>>} -> {ok, Val};
        Else -> Else
    end;

read(This, i64) ->
    case read(This, 8) of
        {ok, <<Val:64/integer-signed-big, _/binary>>} -> {ok, Val};
        Else -> Else
    end;

read(This, double) ->
    case read(This, 8) of
        {ok, <<Val:64/float-signed-big, _/binary>>} -> {ok, Val};
        Else -> Else
    end;

% returns a binary directly, call binary_to_list if necessary
read(This, string) ->
    {ok, Sz}  = read(This, i32),
    {ok, Bin} = read(This, Sz);

read(This, 0) -> {ok, <<>>};
read(This, Len) when is_integer(Len), Len >= 0 ->
    thrift_transport:read(This#binary_protocol.transport, Len).


%%%% FACTORY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(tbp_opts, {strict_read = true,
                   strict_write = true}).

parse_factory_options([], Opts) ->
    Opts;
parse_factory_options([{strict_read, Bool} | Rest], Opts) when is_boolean(Bool) ->
    parse_factory_options(Rest, Opts#tbp_opts{strict_read=Bool});
parse_factory_options([{strict_write, Bool} | Rest], Opts) when is_boolean(Bool) ->
    parse_factory_options(Rest, Opts#tbp_opts{strict_write=Bool}).


%% returns a (fun() -> thrift_protocol())
new_protocol_factory(TransportFactory, Options) ->
    ParsedOpts = parse_factory_options(Options, #tbp_opts{}),
    F = fun() ->
                {ok, Transport} = TransportFactory(),
                thrift_binary_protocol:new(
                  Transport,
                  [{strict_read,  ParsedOpts#tbp_opts.strict_read},
                   {strict_write, ParsedOpts#tbp_opts.strict_write}])
        end,
    {ok, F}.

