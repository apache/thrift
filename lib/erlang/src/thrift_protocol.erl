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
    validate/1,
    flush_transport/1,
    close_transport/1,
    typeid_to_atom/1
]).

-export([behaviour_info/1]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-record(protocol, {module, data}).

behaviour_info(callbacks) ->
    [
        {read, 2},
        {write, 2},
        {flush_transport, 1},
        {close_transport, 1}
    ];
behaviour_info(_Else) ->
    undefined.

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

-spec close_transport(#protocol{}) -> {#protocol{}, _Result}.
close_transport(
    Proto = #protocol{
        module = Module,
        data = Data
    }
) ->
    {Data1, Result} = Module:close_transport(Data),
    {Proto#protocol{data = Data1}, Result}.

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
term_to_typeid({struct, _, _}) -> ?tType_STRUCT;
term_to_typeid({enum, _}) -> ?tType_I32;
term_to_typeid({map, _, _}) -> ?tType_MAP;
term_to_typeid({set, _}) -> ?tType_SET;
term_to_typeid({list, _}) -> ?tType_LIST.

%% Structure is like:
%%    [{Fid, Type}, ...]
-spec read(#protocol{}, {struct, _Flavour, _StructDef}, atom()) ->
    {#protocol{}, {ok, tuple()}}.
read(IProto0, {struct, union, StructDef}, _Tag) when
    is_list(StructDef)
->
    {IProto1, RTuple} = read_union_loop(IProto0, enumerate(1, StructDef)),
    case RTuple of
        [{_, _} = Data] -> {IProto1, {ok, Data}};
        [] -> {IProto1, {ok, empty}};
        [_ | _] -> {IProto1, {ok, {multiple, RTuple}}}
    end;
read(IProto0, {struct, _, StructDef}, Tag) when
    is_list(StructDef), is_atom(Tag)
->
    {IProto1, ok} = read_frag(IProto0, struct_begin),
    {Offset, RTuple0} = construct_default_struct(Tag, StructDef),
    {IProto2, RTuple1} = read_struct_loop(IProto1, enumerate(Offset + 1, StructDef), RTuple0),
    {IProto2, {ok, RTuple1}}.

construct_default_struct(Tag, StructDef) ->
    case Tag of
        undefined ->
            Tuple = erlang:make_tuple(length(StructDef), undefined),
            {0, fill_default_struct(1, StructDef, Tuple)};
        _ ->
            % If we want a tagged tuple, we need to offset all the tuple indices
            % by 1 to avoid overwriting the tag.
            Tuple = erlang:make_tuple(length(StructDef) + 1, undefined),
            {1, fill_default_struct(2, StructDef, erlang:setelement(1, Tuple, Tag))}
    end.

fill_default_struct(_N, [], Record) ->
    Record;
fill_default_struct(N, [{_Fid, _Req, _Type, _Name, Default} | Rest], Record) ->
    fill_default_struct(N + 1, Rest, erlang:setelement(N, Record, Default)).

enumerate(N, [{Fid, _Req, Type, Name, _Default} | Rest]) ->
    [{N, Fid, Type, Name} | enumerate(N + 1, Rest)];
enumerate(_, []) ->
    [].

%% NOTE: Keep this in sync with thrift_protocol_behaviour:read
-spec read
    (#protocol{}, {struct, _Flavour, _Info}) -> {#protocol{}, {ok, tuple()} | {error, _Reason}};
    (#protocol{}, tprot_cont_tag()) -> {#protocol{}, {ok, any()} | {error, _Reason}};
    (#protocol{}, tprot_empty_tag()) -> {#protocol{}, ok | {error, _Reason}};
    (#protocol{}, tprot_header_tag()) -> {#protocol{}, tprot_header_val() | {error, _Reason}};
    (#protocol{}, tprot_data_tag()) -> {#protocol{}, {ok, any()} | {error, _Reason}}.

read(IProto, Type) ->
    case Result = read_frag(IProto, Type) of
        {IProto2, {ok, Data}} ->
            case validate({Type, Data}) of
                ok -> Result;
                Error -> {IProto2, Error}
            end;
        _ ->
            Result
    end.

read_frag(IProto, {struct, union, {Module, StructureName}}) when
    is_atom(Module), is_atom(StructureName)
->
    read(IProto, Module:struct_info(StructureName), undefined);
read_frag(IProto, {struct, _, {Module, StructureName}}) when
    is_atom(Module), is_atom(StructureName)
->
    read(IProto, Module:struct_info(StructureName), Module:record_name(StructureName));
read_frag(IProto, S = {struct, _, Structure}) when is_list(Structure) ->
    read(IProto, S, undefined);
read_frag(IProto, {enum, {Module, EnumName}}) when is_atom(Module) ->
    read_frag(IProto, Module:enum_info(EnumName));
read_frag(IProto, {enum, Fields}) when is_list(Fields) ->
    {IProto2, {ok, IVal}} = read_frag(IProto, i32),
    {EnumVal, IVal} = lists:keyfind(IVal, 2, Fields),
    {IProto2, {ok, EnumVal}};
read_frag(IProto0, {list, Type}) ->
    {IProto1, #protocol_list_begin{etype = EType, size = Size}} =
        read_frag(IProto0, list_begin),
    {EType, EType} = {term_to_typeid(Type), EType},
    {IProto2, List} = read_list_loop(IProto1, Type, Size),
    {IProto3, ok} = read_frag(IProto2, list_end),
    {IProto3, {ok, List}};
read_frag(IProto0, {map, KeyType, ValType}) ->
    {IProto1, #protocol_map_begin{size = Size, ktype = KType, vtype = VType}} =
        read_frag(IProto0, map_begin),
    _ =
        case Size of
            0 ->
                0;
            _ ->
                {KType, KType} = {term_to_typeid(KeyType), KType},
                {VType, VType} = {term_to_typeid(ValType), VType}
        end,
    {IProto2, Map} = read_map_loop(IProto1, KeyType, ValType, Size),
    {IProto3, ok} = read_frag(IProto2, map_end),
    {IProto3, {ok, Map}};
read_frag(IProto0, {set, Type}) ->
    {IProto1, #protocol_set_begin{etype = EType, size = Size}} =
        read_frag(IProto0, set_begin),
    {EType, EType} = {term_to_typeid(Type), EType},
    {IProto2, Set} = read_set_loop(IProto1, Type, Size),
    {IProto3, ok} = read_frag(IProto2, set_end),
    {IProto3, {ok, Set}};
read_frag(Protocol, ProtocolType) ->
    read_specific(Protocol, ProtocolType).

%% NOTE: Keep this in sync with thrift_protocol_behaviour:read
-spec read_specific
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

-spec read_list_loop(#protocol{}, any(), non_neg_integer()) -> {#protocol{}, [any()]}.
read_list_loop(Proto0, ValType, Size) ->
    read_list_loop(Proto0, ValType, Size, []).

read_list_loop(Proto0, _ValType, 0, List) ->
    {Proto0, lists:reverse(List)};
read_list_loop(Proto0, ValType, Left, List) ->
    {Proto1, {ok, Val}} = read_frag(Proto0, ValType),
    read_list_loop(Proto1, ValType, Left - 1, [Val | List]).

-spec read_map_loop(#protocol{}, any(), any(), non_neg_integer()) -> {#protocol{}, map()}.
read_map_loop(Proto0, KeyType, ValType, Size) ->
    read_map_loop(Proto0, KeyType, ValType, Size, #{}).

read_map_loop(Proto0, _KeyType, _ValType, 0, Map) ->
    {Proto0, Map};
read_map_loop(Proto0, KeyType, ValType, Left, Map) ->
    {Proto1, {ok, Key}} = read_frag(Proto0, KeyType),
    {Proto2, {ok, Val}} = read_frag(Proto1, ValType),
    read_map_loop(Proto2, KeyType, ValType, Left - 1, maps:put(Key, Val, Map)).

-spec read_set_loop(#protocol{}, any(), non_neg_integer()) -> {#protocol{}, ordsets:ordset(any())}.
read_set_loop(Proto0, ValType, Size) ->
    read_set_loop(Proto0, ValType, Size, ordsets:new()).

read_set_loop(Proto0, _ValType, 0, Set) ->
    {Proto0, Set};
read_set_loop(Proto0, ValType, Left, Set) ->
    {Proto1, {ok, Val}} = read_frag(Proto0, ValType),
    read_set_loop(Proto1, ValType, Left - 1, ordsets:add_element(Val, Set)).

read_struct_loop(IProto0, StructIndex, RTuple) ->
    read_struct_loop(
        IProto0,
        StructIndex,
        fun({N, _, Val}, Acc) -> setelement(N, Acc, Val) end,
        RTuple
    ).

read_union_loop(IProto0, StructIndex) ->
    read_struct_loop(
        IProto0,
        StructIndex,
        fun({_, Name, Val}, Was) -> [{Name, Val} | Was] end,
        []
    ).

read_struct_loop(IProto0, StructIndex, Fun, Acc) ->
    {IProto1, #protocol_field_begin{type = FType, id = Fid}} = read_frag(IProto0, field_begin),
    case {FType, Fid} of
        {?tType_STOP, _} ->
            {IProto2, ok} = read_frag(IProto1, struct_end),
            {IProto2, Acc};
        _Else ->
            case lists:keyfind(Fid, 2, StructIndex) of
                {N, Fid, Type, Name} ->
                    case term_to_typeid(Type) of
                        FType ->
                            {IProto2, {ok, Val}} = read_frag(IProto1, Type),
                            {IProto3, ok} = read_frag(IProto2, field_end),
                            NewAcc = Fun({N, Name, Val}, Acc),
                            read_struct_loop(IProto3, StructIndex, Fun, NewAcc);
                        _Expected ->
                            error_logger:info_msg(
                                "Skipping field ~p with wrong type: ~p~n",
                                [Name, typeid_to_atom(FType)]
                            ),
                            skip_field(FType, IProto1, StructIndex, Acc)
                    end;
                false ->
                    error_logger:info_msg(
                        "Skipping unknown field [~p] with type: ~p~n",
                        [Fid, typeid_to_atom(FType)]
                    ),
                    skip_field(FType, IProto1, StructIndex, Acc)
            end
    end.

skip_field(FType, IProto0, StructIndex, Acc) ->
    FTypeAtom = typeid_to_atom(FType),
    {IProto1, ok} = skip(IProto0, FTypeAtom),
    {IProto2, ok} = read_frag(IProto1, field_end),
    read_struct_loop(IProto2, StructIndex, Acc).

-spec skip(#protocol{}, any()) -> {#protocol{}, ok}.

skip(Proto0, struct) ->
    {Proto1, ok} = read_frag(Proto0, struct_begin),
    {Proto2, ok} = skip_struct_loop(Proto1),
    {Proto3, ok} = read_frag(Proto2, struct_end),
    {Proto3, ok};
skip(Proto0, map) ->
    {Proto1, Map} = read_frag(Proto0, map_begin),
    {Proto2, ok} = skip_map_loop(Proto1, Map),
    {Proto3, ok} = read_frag(Proto2, map_end),
    {Proto3, ok};
skip(Proto0, set) ->
    {Proto1, Set} = read_frag(Proto0, set_begin),
    {Proto2, ok} = skip_set_loop(Proto1, Set),
    {Proto3, ok} = read_frag(Proto2, set_end),
    {Proto3, ok};
skip(Proto0, list) ->
    {Proto1, List} = read_frag(Proto0, list_begin),
    {Proto2, ok} = skip_list_loop(Proto1, List),
    {Proto3, ok} = read_frag(Proto2, list_end),
    {Proto3, ok};
skip(Proto0, Type) when is_atom(Type) ->
    {Proto1, _Ignore} = read_frag(Proto0, Type),
    {Proto1, ok};
skip(Proto0, Type) when is_integer(Type) ->
    skip(Proto0, typeid_to_atom(Type)).

skip_struct_loop(Proto0) ->
    {Proto1, #protocol_field_begin{type = Type}} = read(Proto0, field_begin),
    case Type of
        ?tType_STOP ->
            {Proto1, ok};
        _Else ->
            {Proto2, ok} = skip(Proto1, Type),
            {Proto3, ok} = read(Proto2, field_end),
            skip_struct_loop(Proto3)
    end.

skip_map_loop(Proto0, #protocol_map_begin{size = 0}) ->
    {Proto0, ok};
skip_map_loop(Proto0, Map = #protocol_map_begin{ktype = Ktype, vtype = Vtype, size = Size}) ->
    {Proto1, ok} = skip(Proto0, Ktype),
    {Proto2, ok} = skip(Proto1, Vtype),
    skip_map_loop(Proto2, Map#protocol_map_begin{size = Size - 1}).

skip_set_loop(Proto0, #protocol_set_begin{size = 0}) ->
    {Proto0, ok};
skip_set_loop(Proto0, Map = #protocol_set_begin{etype = Etype, size = Size}) ->
    {Proto1, ok} = skip(Proto0, Etype),
    skip_set_loop(Proto1, Map#protocol_set_begin{size = Size - 1}).

skip_list_loop(Proto0, #protocol_list_begin{size = 0}) ->
    {Proto0, ok};
skip_list_loop(Proto0, Map = #protocol_list_begin{etype = Etype, size = Size}) ->
    {Proto1, ok} = skip(Proto0, Etype),
    skip_list_loop(Proto1, Map#protocol_list_begin{size = Size - 1}).

%%--------------------------------------------------------------------
%% Function: write(OProto, {Type, Data}) -> ok
%%
%% Type = {struct, Flavour, StructDef} |
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

write(Proto, TypeData) ->
    case validate(TypeData) of
        ok -> write_frag(Proto, TypeData);
        Error -> {Proto, Error}
    end.

write_frag(Proto0, {{struct, union, StructDef}, {Name, Value}}, StructName) when
    is_list(StructDef)
->
    {Proto1, ok} = write_frag(Proto0, #protocol_struct_begin{name = StructName}),
    {Proto2, ok} = struct_write_loop(Proto1, [lists:keyfind(Name, 4, StructDef)], [Value]),
    {Proto3, ok} = write_frag(Proto2, struct_end),
    {Proto3, ok};
write_frag(Proto0, {{struct, _, StructDef}, Data}, StructName) when
    is_list(StructDef), is_tuple(Data), length(StructDef) == size(Data) - 1
->
    [_ | Elems] = tuple_to_list(Data),
    {Proto1, ok} = write_frag(Proto0, #protocol_struct_begin{name = StructName}),
    {Proto2, ok} = struct_write_loop(Proto1, StructDef, Elems),
    {Proto3, ok} = write_frag(Proto2, struct_end),
    {Proto3, ok}.

%% thrift client specific stuff
write_frag(Proto0, {{struct, _, StructDef}, Data}) when
    is_list(StructDef), is_tuple(Data), length(StructDef) == size(Data) - 1
->
    [StructName | Elems] = tuple_to_list(Data),
    {Proto1, ok} = write_frag(Proto0, #protocol_struct_begin{name = StructName}),
    {Proto2, ok} = struct_write_loop(Proto1, StructDef, Elems),
    {Proto3, ok} = write_frag(Proto2, struct_end),
    {Proto3, ok};
write_frag(Proto, {{struct, _, {Module, StructureName}}, Data}) when
    is_atom(Module),
    is_atom(StructureName)
->
    write_frag(Proto, {Module:struct_info(StructureName), Data}, StructureName);
write_frag(Proto, {{enum, Fields}, Data}) when is_list(Fields), is_atom(Data) ->
    {Data, IVal} = lists:keyfind(Data, 1, Fields),
    write_frag(Proto, {i32, IVal});
write_frag(Proto, {{enum, {Module, EnumName}}, Data}) when
    is_atom(Module),
    is_atom(EnumName)
->
    write_frag(Proto, {Module:enum_info(EnumName), Data});
write_frag(Proto0, {{list, Type}, Data}) when
    is_list(Data)
->
    {Proto1, ok} = write_frag(
        Proto0,
        #protocol_list_begin{
            etype = term_to_typeid(Type),
            size = length(Data)
        }
    ),
    Proto2 = lists:foldl(
        fun(Elem, ProtoIn) ->
            {ProtoOut, ok} = write_frag(ProtoIn, {Type, Elem}),
            ProtoOut
        end,
        Proto1,
        Data
    ),
    {Proto3, ok} = write_frag(Proto2, list_end),
    {Proto3, ok};
write_frag(Proto0, {{map, KeyType, ValType}, Data}) ->
    {Proto1, ok} = write_frag(
        Proto0,
        #protocol_map_begin{
            ktype = term_to_typeid(KeyType),
            vtype = term_to_typeid(ValType),
            size = map_size(Data)
        }
    ),
    Proto2 = maps:fold(
        fun(KeyData, ValData, ProtoS0) ->
            {ProtoS1, ok} = write_frag(ProtoS0, {KeyType, KeyData}),
            {ProtoS2, ok} = write_frag(ProtoS1, {ValType, ValData}),
            ProtoS2
        end,
        Proto1,
        Data
    ),
    {Proto3, ok} = write_frag(Proto2, map_end),
    {Proto3, ok};
write_frag(Proto0, {{set, Type}, Data}) ->
    true = ordsets:is_set(Data),
    {Proto1, ok} = write_frag(
        Proto0,
        #protocol_set_begin{
            etype = term_to_typeid(Type),
            size = ordsets:size(Data)
        }
    ),
    Proto2 = ordsets:fold(
        fun(Elem, ProtoIn) ->
            {ProtoOut, ok} = write_frag(ProtoIn, {Type, Elem}),
            ProtoOut
        end,
        Proto1,
        Data
    ),
    {Proto3, ok} = write_frag(Proto2, set_end),
    {Proto3, ok};
write_frag(
    Proto = #protocol{
        module = Module,
        data = ModuleData
    },
    Data
) ->
    {NewData, Result} = Module:write(ModuleData, Data),
    {Proto#protocol{data = NewData}, Result}.

struct_write_loop(Proto0, [{Fid, _Req, Type, _Name, _Default} | RestStructDef], [Data | RestData]) ->
    NewProto =
        case Data of
            undefined ->
                % null fields are skipped in response
                Proto0;
            _ ->
                {Proto1, ok} = write_frag(
                    Proto0,
                    #protocol_field_begin{
                        type = term_to_typeid(Type),
                        id = Fid
                    }
                ),
                {Proto2, ok} = write_frag(Proto1, {Type, Data}),
                {Proto3, ok} = write_frag(Proto2, field_end),
                Proto3
        end,
    struct_write_loop(NewProto, RestStructDef, RestData);
struct_write_loop(Proto, [], []) ->
    write_frag(Proto, field_stop).

-spec validate(
    tprot_header_val() | tprot_header_tag() | tprot_empty_tag() | field_stop | TypeData
) ->
    ok | {error, {invalid, Location :: [atom()], Value :: term()}}
when
    TypeData :: {Type, Data},
    Type :: tprot_data_tag() | tprot_cont_tag() | {enum, _Def} | {struct, _Flavour, _Def},
    Data :: term().

validate(#protocol_message_begin{}) ->
    ok;
validate(#protocol_struct_begin{}) ->
    ok;
validate(#protocol_field_begin{}) ->
    ok;
validate(#protocol_map_begin{}) ->
    ok;
validate(#protocol_list_begin{}) ->
    ok;
validate(#protocol_set_begin{}) ->
    ok;
validate(message_end) ->
    ok;
validate(field_stop) ->
    ok;
validate(field_end) ->
    ok;
validate(struct_end) ->
    ok;
validate(list_end) ->
    ok;
validate(set_end) ->
    ok;
validate(map_end) ->
    ok;
validate(TypeData) ->
    try
        validate(TypeData, [])
    catch
        throw:{invalid, Path, _Type, Value} ->
            {error, {invalid, lists:reverse(Path), Value}}
    end.

validate(TypeData, Path) ->
    validate(required, TypeData, Path).

validate(Req, {_Type, undefined}, _Path) when
    Req =:= optional orelse Req =:= undefined
->
    ok;
validate(_Req, {{list, Type}, Data}, Path) when
    is_list(Data)
->
    lists:foreach(fun(E) -> validate({Type, E}, Path) end, Data);
validate(_Req, {{set, Type}, Data}, Path) when
    is_list(Data)
->
    lists:foreach(fun(E) -> validate({Type, E}, Path) end, (ordsets:to_list(Data)));
validate(_Req, {{map, KType, VType}, Data}, Path) when
    is_map(Data)
->
    maps:fold(
        fun(K, V, _) ->
            validate({KType, K}, Path),
            validate({VType, V}, Path),
            ok
        end,
        ok,
        Data
    );
validate(Req, {{struct, union, {Mod, Name}}, Data = {_, _}}, Path) ->
    validate(Req, {Mod:struct_info(Name), Data}, Path);
validate(_Req, {{struct, union, StructDef} = Type, Data = {Name, Value}}, Path) when
    is_list(StructDef) andalso is_atom(Name)
->
    case lists:keyfind(Name, 4, StructDef) of
        {_, _, SubType, Name, _Default} ->
            validate(required, {SubType, Value}, [Name | Path]);
        false ->
            throw({invalid, Path, Type, Data})
    end;
validate(Req, {{struct, _Flavour, {Mod, Name} = Type}, Data}, Path) when
    is_tuple(Data)
->
    try Mod:record_name(Name) of
        RName when RName =:= element(1, Data) ->
            validate(Req, {Mod:struct_info(Name), Data}, Path);
        _ ->
            throw({invalid, Path, Type, Data})
    catch
        error:badarg ->
            throw({invalid, Path, Type, Data})
    end;
validate(_Req, {{struct, _Flavour, StructDef}, Data}, Path) when
    is_list(StructDef) andalso tuple_size(Data) =:= length(StructDef) + 1
->
    [_ | Elems] = tuple_to_list(Data),
    validate_struct_fields(StructDef, Elems, Path);
validate(_Req, {{struct, _Flavour, StructDef}, Data}, Path) when
    is_list(StructDef) andalso tuple_size(Data) =:= length(StructDef)
->
    validate_struct_fields(StructDef, tuple_to_list(Data), Path);
validate(_Req, {{enum, _Fields}, Value}, _Path) when is_atom(Value), Value =/= undefined ->
    ok;
validate(_Req, {string, Value}, _Path) when is_binary(Value) ->
    ok;
validate(_Req, {bool, Value}, _Path) when is_boolean(Value) ->
    ok;
validate(_Req, {byte, Value}, _Path) when
    is_integer(Value), Value >= -(1 bsl 7), Value < (1 bsl 7)
->
    ok;
validate(_Req, {i8, Value}, _Path) when
    is_integer(Value), Value >= -(1 bsl 7), Value < (1 bsl 7)
->
    ok;
validate(_Req, {i16, Value}, _Path) when
    is_integer(Value), Value >= -(1 bsl 15), Value < (1 bsl 15)
->
    ok;
validate(_Req, {i32, Value}, _Path) when
    is_integer(Value), Value >= -(1 bsl 31), Value < (1 bsl 31)
->
    ok;
validate(_Req, {i64, Value}, _Path) when
    is_integer(Value), Value >= -(1 bsl 63), Value < (1 bsl 63)
->
    ok;
validate(_Req, {double, Value}, _Path) when is_float(Value) ->
    ok;
validate(_Req, {Type, Value}, Path) ->
    throw({invalid, Path, Type, Value}).

validate_struct_fields(Types, Elems, Path) ->
    lists:foreach(
        fun({{_, Req, Type, Name, _}, Data}) ->
            validate(Req, {Type, Data}, [Name | Path])
        end,
        lists:zip(Types, Elems)
    ).
