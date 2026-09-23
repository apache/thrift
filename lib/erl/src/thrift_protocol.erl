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

-module(thrift_protocol).

-export([
    new/2,
    write/2,
    read/2,
    read/3,
    skip/2,
    skip/3,
    flush_transport/1,
    close_transport/1,
    typeid_to_atom/1
]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-record(protocol, {
    module :: module(),
    data :: term()
}).

%%%=========================================================================
%%%  API
%%%=========================================================================
-type state() :: term().
-export_type([state/0]).
-type reason() :: term().
-export_type([reason/0]).

%% NOTE: keep this in sync with read/2 spec
-callback read
    (state(), {struct, _Info}) -> {state(), {ok, tuple()} | {error, reason()}};
    (state(), tprot_cont_tag()) -> {state(), {ok, any()} | {error, reason()}};
    (state(), tprot_empty_tag()) -> {state(), ok | {error, reason()}};
    (state(), tprot_header_tag()) -> {state(), tprot_header_val() | {error, reason()}};
    (state(), tprot_data_tag()) -> {state(), {ok, any()} | {error, reason()}}.

-callback write(state(), any()) -> {state(), ok | {error, reason()}}.

-callback flush_transport(state()) -> {state(), ok | {error, reason()}}.
-callback close_transport(state()) -> {state(), ok | {error, reason()}}.

new(Module, Data) when is_atom(Module) ->
    {ok, #protocol{
        module = Module,
        data = Data
    }}.

-spec flush_transport(#protocol{}) -> {#protocol{}, ok}.
flush_transport(
    Proto = #protocol{
        module = Module,
        data = Data
    }
) ->
    {NewData, Result} = Module:flush_transport(Data),
    {Proto#protocol{data = NewData}, Result}.

-spec close_transport(#protocol{}) -> ok.
close_transport(#protocol{
    module = Module,
    data = Data
}) ->
    Module:close_transport(Data).

typeid_to_atom(?tType_STOP) -> field_stop;
typeid_to_atom(?tType_VOID) -> void;
typeid_to_atom(?tType_BOOL) -> bool;
typeid_to_atom(?tType_DOUBLE) -> double;
typeid_to_atom(?tType_I8) -> byte;
typeid_to_atom(?tType_I16) -> i16;
typeid_to_atom(?tType_I32) -> i32;
typeid_to_atom(?tType_I64) -> i64;
typeid_to_atom(?tType_STRING) -> string;
typeid_to_atom(?tType_STRUCT) -> struct;
typeid_to_atom(?tType_MAP) -> map;
typeid_to_atom(?tType_SET) -> set;
typeid_to_atom(?tType_LIST) -> list.

term_to_typeid(void) -> ?tType_VOID;
term_to_typeid(bool) -> ?tType_BOOL;
term_to_typeid(byte) -> ?tType_I8;
term_to_typeid(double) -> ?tType_DOUBLE;
term_to_typeid(i8) -> ?tType_I8;
term_to_typeid(i16) -> ?tType_I16;
term_to_typeid(i32) -> ?tType_I32;
term_to_typeid(i64) -> ?tType_I64;
term_to_typeid(string) -> ?tType_STRING;
term_to_typeid({struct, _}) -> ?tType_STRUCT;
term_to_typeid({map, _, _}) -> ?tType_MAP;
term_to_typeid({set, _}) -> ?tType_SET;
term_to_typeid({list, _}) -> ?tType_LIST.

%% Structure is like:
%%    [{Fid, Type}, ...]
-spec read(#protocol{}, {struct, _StructDef}, atom()) -> {#protocol{}, {ok, tuple()}}.
read(IProto0, {struct, Structure}, Tag) when
    is_list(Structure), is_atom(Tag)
->
    read_struct(IProto0, {struct, Structure}, Tag, ?DEFAULT_RECURSION_DEPTH).

%% NOTE: Keep this in sync with read callback
-spec read
    (#protocol{}, {struct, _Info}) -> {#protocol{}, {ok, tuple()} | {error, _Reason}};
    (#protocol{}, tprot_cont_tag()) -> {#protocol{}, {ok, any()} | {error, _Reason}};
    (#protocol{}, tprot_empty_tag()) -> {#protocol{}, ok | {error, _Reason}};
    (#protocol{}, tprot_header_tag()) -> {#protocol{}, tprot_header_val() | {error, _Reason}};
    (#protocol{}, tprot_data_tag()) -> {#protocol{}, {ok, any()} | {error, _Reason}}.

read(IProto, {struct, {Module, StructureName}}) when
    is_atom(Module),
    is_atom(StructureName)
->
    read_type(IProto, {struct, {Module, StructureName}}, ?DEFAULT_RECURSION_DEPTH);
read(IProto, S = {struct, Structure}) when is_list(Structure) ->
    read_type(IProto, S, ?DEFAULT_RECURSION_DEPTH);
read(IProto0, T = {list, _Type}) ->
    read_type(IProto0, T, ?DEFAULT_RECURSION_DEPTH);
read(IProto0, T = {map, _KeyType, _ValType}) ->
    read_type(IProto0, T, ?DEFAULT_RECURSION_DEPTH);
read(IProto0, T = {set, _Type}) ->
    read_type(IProto0, T, ?DEFAULT_RECURSION_DEPTH);
read(Protocol, ProtocolType) ->
    read_specific(Protocol, ProtocolType).

%% Read a value whose type comes from the generated struct_info rather than
%% from the wire. A {struct, {Module, Name}} reference is lazy, so a schema
%% whose types refer back to themselves recurses one level per wire nesting
%% level. Each level of composite nesting spends one unit of the same
%% allowance skip/3 uses, seeded at ?DEFAULT_RECURSION_DEPTH, and exceeding it
%% raises the same error. Leaf types carry no allowance because they cannot
%% nest. Kept in step with the struct, list, map and set clauses of read/2.
read_type(_IProto, _Type, Depth) when Depth =< 0 ->
    error({protocol_error, max_skip_depth_exceeded});
read_type(IProto, {struct, {Module, StructureName}}, Depth) when
    is_atom(Module),
    is_atom(StructureName)
->
    read_struct(IProto, Module:struct_info(StructureName), StructureName, Depth);
read_type(IProto, S = {struct, Structure}, Depth) when is_list(Structure) ->
    read_struct(IProto, S, undefined, Depth);
read_type(IProto0, {list, Type}, Depth) ->
    {IProto1, #protocol_list_begin{etype = EType, size = Size}} =
        read(IProto0, list_begin),
    {EType, EType} = {term_to_typeid(Type), EType},
    {List, IProto2} = read_container_loop(
        IProto1,
        fun(ProtoS0) ->
            {ProtoS1, {ok, Item}} = read_type(ProtoS0, Type, Depth - 1),
            {Item, ProtoS1}
        end,
        Size,
        []
    ),
    {IProto3, ok} = read(IProto2, list_end),
    {IProto3, {ok, List}};
read_type(IProto0, {map, KeyType, ValType}, Depth) ->
    {IProto1, #protocol_map_begin{size = Size, ktype = KType, vtype = VType}} =
        read(IProto0, map_begin),
    _ =
        case Size of
            0 ->
                0;
            _ ->
                {KType, KType} = {term_to_typeid(KeyType), KType},
                {VType, VType} = {term_to_typeid(ValType), VType}
        end,
    {List, IProto2} = read_container_loop(
        IProto1,
        fun(ProtoS0) ->
            {ProtoS1, {ok, Key}} = read_type(ProtoS0, KeyType, Depth - 1),
            {ProtoS2, {ok, Val}} = read_type(ProtoS1, ValType, Depth - 1),
            {{Key, Val}, ProtoS2}
        end,
        Size,
        []
    ),
    {IProto3, ok} = read(IProto2, map_end),
    {IProto3, {ok, dict:from_list(List)}};
read_type(IProto0, {set, Type}, Depth) ->
    {IProto1, #protocol_set_begin{etype = EType, size = Size}} =
        read(IProto0, set_begin),
    {EType, EType} = {term_to_typeid(Type), EType},
    {List, IProto2} = read_container_loop(
        IProto1,
        fun(ProtoS0) ->
            {ProtoS1, {ok, Item}} = read_type(ProtoS0, Type, Depth - 1),
            {Item, ProtoS1}
        end,
        Size,
        []
    ),
    {IProto3, ok} = read(IProto2, set_end),
    {IProto3, {ok, sets:from_list(List)}};
read_type(Protocol, ProtocolType, _Depth) ->
    read_specific(Protocol, ProtocolType).

read_struct(IProto0, {struct, Structure}, Tag, Depth) when
    is_list(Structure)
->
    % If we want a tagged tuple, we need to offset all the tuple indices
    % by 1 to avoid overwriting the tag.
    Offset =
        if
            Tag =/= undefined -> 1;
            true -> 0
        end,
    IndexList =
        case length(Structure) of
            N when N > 0 -> lists:seq(1 + Offset, N + Offset);
            _ -> []
        end,

    SWithIndices = [
        {Fid, {Type, Index}}
     || {{Fid, Type}, Index} <-
            lists:zip(Structure, IndexList)
    ],
    % Fid -> {Type, Index}
    SDict = dict:from_list(SWithIndices),

    {IProto1, ok} = read(IProto0, struct_begin),
    RTuple0 = erlang:make_tuple(length(Structure) + Offset, undefined),
    RTuple1 =
        if
            Tag =/= undefined -> setelement(1, RTuple0, Tag);
            true -> RTuple0
        end,

    {IProto2, RTuple2} = read_struct_loop(IProto1, SDict, RTuple1, Depth),
    {IProto2, {ok, RTuple2}}.

%% Reads N container items one at a time, threading the protocol state and
%% accumulating the results. A container header carries its element count, which
%% is read before any element, so a peer can name a count it never backs with
%% data. Reading incrementally keeps the cost proportional to the elements
%% actually present, rather than materialising an N-element driver list
%% (lists:duplicate/2) from the wire count up front. ReadItemFun(Proto) returns
%% {Item, Proto1}.
read_container_loop(Proto, _ReadItemFun, 0, Acc) ->
    {lists:reverse(Acc), Proto};
read_container_loop(Proto, ReadItemFun, N, Acc) when N > 0 ->
    {Item, Proto1} = ReadItemFun(Proto),
    read_container_loop(Proto1, ReadItemFun, N - 1, [Item | Acc]).

%% NOTE: Keep this in sync with read/2 spec
-spec read_specific
    (#protocol{}, {struct, _Info}) -> {#protocol{}, {ok, tuple()} | {error, _Reason}};
    (#protocol{}, tprot_cont_tag()) -> {#protocol{}, {ok, any()} | {error, _Reason}};
    (#protocol{}, tprot_empty_tag()) -> {#protocol{}, ok | {error, _Reason}};
    (#protocol{}, tprot_header_tag()) -> {#protocol{}, tprot_header_val() | {error, _Reason}};
    (#protocol{}, tprot_data_tag()) -> {#protocol{}, {ok, any()} | {error, _Reason}}.
read_specific(
    Proto = #protocol{
        module = Module,
        data = ModuleData
    },
    ProtocolType
) ->
    {NewData, Result} = Module:read(ModuleData, ProtocolType),
    {Proto#protocol{data = NewData}, Result}.

read_struct_loop(IProto0, SDict, RTuple, Depth) ->
    {IProto1, #protocol_field_begin{type = FType, id = Fid}} =
        thrift_protocol:read(IProto0, field_begin),
    case {FType, Fid} of
        {?tType_STOP, _} ->
            {IProto2, ok} = read(IProto1, struct_end),
            {IProto2, RTuple};
        _Else ->
            case dict:find(Fid, SDict) of
                {ok, {Type, Index}} ->
                    case term_to_typeid(Type) of
                        FType ->
                            {IProto2, {ok, Val}} = read_type(IProto1, Type, Depth - 1),
                            {IProto3, ok} = thrift_protocol:read(IProto2, field_end),
                            NewRTuple = setelement(Index, RTuple, Val),
                            read_struct_loop(IProto3, SDict, NewRTuple, Depth);
                        Expected ->
                            error_logger:info_msg(
                                "Skipping field ~p with wrong type (~p != ~p)~n",
                                [Fid, FType, Expected]
                            ),
                            skip_field(FType, IProto1, SDict, RTuple, Depth)
                    end;
                _Else2 ->
                    skip_field(FType, IProto1, SDict, RTuple, Depth)
            end
    end.

skip_field(FType, IProto0, SDict, RTuple, Depth) ->
    {IProto1, ok} = skip(IProto0, typeid_to_atom(FType)),
    {IProto2, ok} = read(IProto1, field_end),
    read_struct_loop(IProto2, SDict, RTuple, Depth).

-spec skip(#protocol{}, atom()) -> {#protocol{}, ok}.

%% What this walks is decided by type ids taken off the wire rather than by
%% the IDL, so the peer picks the nesting and pays three bytes a level for it.
%% The allowance starts fresh here: this is a separate walk from the one
%% read_type/3 charges, and the two do not nest into a single unbounded chain
%% because a field is either read by its declared type or skipped, never both.
skip(Proto, Type) ->
    skip(Proto, Type, ?DEFAULT_RECURSION_DEPTH).

-spec skip(#protocol{}, atom(), integer()) -> {#protocol{}, ok}.

skip(_Proto, _Type, Depth) when Depth =< 0 ->
    error({protocol_error, max_skip_depth_exceeded});
skip(Proto0, struct, Depth) ->
    {Proto1, ok} = read(Proto0, struct_begin),
    {Proto2, ok} = skip_struct_loop(Proto1, Depth),
    {Proto3, ok} = read(Proto2, struct_end),
    {Proto3, ok};
skip(Proto0, map, Depth) ->
    {Proto1, Map} = read(Proto0, map_begin),
    {Proto2, ok} = skip_map_loop(Proto1, Map, Depth),
    {Proto3, ok} = read(Proto2, map_end),
    {Proto3, ok};
skip(Proto0, set, Depth) ->
    {Proto1, Set} = read(Proto0, set_begin),
    {Proto2, ok} = skip_set_loop(Proto1, Set, Depth),
    {Proto3, ok} = read(Proto2, set_end),
    {Proto3, ok};
skip(Proto0, list, Depth) ->
    {Proto1, List} = read(Proto0, list_begin),
    {Proto2, ok} = skip_list_loop(Proto1, List, Depth),
    {Proto3, ok} = read(Proto2, list_end),
    {Proto3, ok};
skip(Proto0, Type, _Depth) when is_atom(Type) ->
    {Proto1, _Ignore} = read(Proto0, Type),
    {Proto1, ok}.

%% The loops below recurse once per element, which is a tail call and costs no
%% stack, and once per level of nesting, which does. Only the second dimension
%% spends the allowance.

skip_struct_loop(Proto0, Depth) ->
    {Proto1, #protocol_field_begin{type = Type}} = read(Proto0, field_begin),
    case Type of
        ?tType_STOP ->
            {Proto1, ok};
        _Else ->
            {Proto2, ok} = skip(Proto1, typeid_to_atom(Type), Depth - 1),
            {Proto3, ok} = read(Proto2, field_end),
            skip_struct_loop(Proto3, Depth)
    end.

skip_map_loop(
    Proto0,
    Map = #protocol_map_begin{
        ktype = Ktype,
        vtype = Vtype,
        size = Size
    },
    Depth
) ->
    case Size of
        N when N > 0 ->
            {Proto1, ok} = skip(Proto0, typeid_to_atom(Ktype), Depth - 1),
            {Proto2, ok} = skip(Proto1, typeid_to_atom(Vtype), Depth - 1),
            skip_map_loop(
                Proto2,
                Map#protocol_map_begin{size = Size - 1},
                Depth
            );
        0 ->
            {Proto0, ok}
    end.

skip_set_loop(
    Proto0,
    Map = #protocol_set_begin{
        etype = Etype,
        size = Size
    },
    Depth
) ->
    case Size of
        N when N > 0 ->
            {Proto1, ok} = skip(Proto0, typeid_to_atom(Etype), Depth - 1),
            skip_set_loop(
                Proto1,
                Map#protocol_set_begin{size = Size - 1},
                Depth
            );
        0 ->
            {Proto0, ok}
    end.

skip_list_loop(
    Proto0,
    Map = #protocol_list_begin{
        etype = Etype,
        size = Size
    },
    Depth
) ->
    case Size of
        N when N > 0 ->
            {Proto1, ok} = skip(Proto0, typeid_to_atom(Etype), Depth - 1),
            skip_list_loop(
                Proto1,
                Map#protocol_list_begin{size = Size - 1},
                Depth
            );
        0 ->
            {Proto0, ok}
    end.

%%--------------------------------------------------------------------
%% Function: write(OProto, {Type, Data}) -> ok
%%
%% Type = {struct, StructDef} |
%%        {list, Type} |
%%        {map, KeyType, ValType} |
%%        {set, Type} |
%%        BaseType
%%
%% Data =
%%         tuple()  -- for struct
%%       | list()   -- for list
%%       | dictionary()   -- for map
%%       | set()    -- for set
%%       | any()    -- for base types
%%
%% Description:
%%--------------------------------------------------------------------
-spec write(#protocol{}, any()) -> {#protocol{}, ok | {error, _Reason}}.

write(Proto0, {{struct, StructDef}, Data}) when
    is_list(StructDef), is_tuple(Data), length(StructDef) == size(Data) - 1
->
    [StructName | Elems] = tuple_to_list(Data),
    {Proto1, ok} = write(Proto0, #protocol_struct_begin{name = StructName}),
    {Proto2, ok} = struct_write_loop(Proto1, StructDef, Elems),
    {Proto3, ok} = write(Proto2, struct_end),
    {Proto3, ok};
write(Proto, {{struct, {Module, StructureName}}, Data}) when
    is_atom(Module),
    is_atom(StructureName),
    element(1, Data) =:= StructureName
->
    write(Proto, {Module:struct_info(StructureName), Data});
write(_, {{struct, {Module, StructureName}}, Data}) when
    is_atom(Module),
    is_atom(StructureName)
->
    erlang:error(struct_unmatched, {{provided, element(1, Data)}, {expected, StructureName}});
write(Proto0, {{list, Type}, Data}) when
    is_list(Data)
->
    {Proto1, ok} = write(
        Proto0,
        #protocol_list_begin{
            etype = term_to_typeid(Type),
            size = length(Data)
        }
    ),
    Proto2 = lists:foldl(
        fun(Elem, ProtoIn) ->
            {ProtoOut, ok} = write(ProtoIn, {Type, Elem}),
            ProtoOut
        end,
        Proto1,
        Data
    ),
    {Proto3, ok} = write(Proto2, list_end),
    {Proto3, ok};
write(Proto0, {{map, KeyType, ValType}, Data}) ->
    {Proto1, ok} = write(
        Proto0,
        #protocol_map_begin{
            ktype = term_to_typeid(KeyType),
            vtype = term_to_typeid(ValType),
            size = dict:size(Data)
        }
    ),
    Proto2 = dict:fold(
        fun(KeyData, ValData, ProtoS0) ->
            {ProtoS1, ok} = write(ProtoS0, {KeyType, KeyData}),
            {ProtoS2, ok} = write(ProtoS1, {ValType, ValData}),
            ProtoS2
        end,
        Proto1,
        Data
    ),
    {Proto3, ok} = write(Proto2, map_end),
    {Proto3, ok};
write(Proto0, {{set, Type}, Data}) ->
    true = sets:is_set(Data),
    {Proto1, ok} = write(
        Proto0,
        #protocol_set_begin{
            etype = term_to_typeid(Type),
            size = sets:size(Data)
        }
    ),
    Proto2 = sets:fold(
        fun(Elem, ProtoIn) ->
            {ProtoOut, ok} = write(ProtoIn, {Type, Elem}),
            ProtoOut
        end,
        Proto1,
        Data
    ),
    {Proto3, ok} = write(Proto2, set_end),
    {Proto3, ok};
write(
    Proto = #protocol{
        module = Module,
        data = ModuleData
    },
    Data
) ->
    {NewData, Result} = Module:write(ModuleData, Data),
    {Proto#protocol{data = NewData}, Result}.

struct_write_loop(Proto0, [{Fid, Type} | RestStructDef], [Data | RestData]) ->
    NewProto =
        case Data of
            undefined ->
                % null fields are skipped in response
                Proto0;
            _ ->
                {Proto1, ok} = write(
                    Proto0,
                    #protocol_field_begin{
                        type = term_to_typeid(Type),
                        id = Fid
                    }
                ),
                {Proto2, ok} = write(Proto1, {Type, Data}),
                {Proto3, ok} = write(Proto2, field_end),
                Proto3
        end,
    struct_write_loop(NewProto, RestStructDef, RestData);
struct_write_loop(Proto, [], []) ->
    write(Proto, field_stop).
