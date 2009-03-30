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

-export([new/2,
         write/2,
         read/2,
         read/3,
         skip/2,
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
behaviour_info(_Else) -> undefined.

new(Module, Data) when is_atom(Module) ->
    {ok, #protocol{module = Module,
                   data = Data}}.

flush_transport(#protocol{module = Module,
                          data = Data}) ->
    Module:flush_transport(Data).

close_transport(#protocol{module = Module,
                          data = Data}) ->
    Module:close_transport(Data).

typeid_to_atom(?tType_STOP) -> field_stop;
typeid_to_atom(?tType_VOID) -> void;
typeid_to_atom(?tType_BOOL) -> bool;
typeid_to_atom(?tType_BYTE) -> byte;
typeid_to_atom(?tType_DOUBLE) -> double;
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
term_to_typeid(byte) -> ?tType_BYTE;
term_to_typeid(double) -> ?tType_DOUBLE;
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
read(IProto, {struct, Structure}, Tag)
  when is_list(Structure), is_atom(Tag) ->

    % If we want a tagged tuple, we need to offset all the tuple indices
    % by 1 to avoid overwriting the tag.
    Offset = if Tag =/= undefined -> 1; true -> 0 end,
    IndexList = case length(Structure) of
                    N when N > 0 -> lists:seq(1 + Offset, N + Offset);
                    _ -> []
                end,

    SWithIndices = [{Fid, {Type, Index}} ||
                       {{Fid, Type}, Index} <-
                           lists:zip(Structure, IndexList)],
    % Fid -> {Type, Index}
    SDict = dict:from_list(SWithIndices),

    ok = read(IProto, struct_begin),
    RTuple0 = erlang:make_tuple(length(Structure) + Offset, undefined),
    RTuple1 = if Tag =/= undefined -> setelement(1, RTuple0, Tag);
                 true              -> RTuple0
              end,

    RTuple2 = read_struct_loop(IProto, SDict, RTuple1),
    {ok, RTuple2}.

read(IProto, {struct, {Module, StructureName}}) when is_atom(Module),
                                                     is_atom(StructureName) ->
    read(IProto, Module:struct_info(StructureName), StructureName);

read(IProto, S={struct, Structure}) when is_list(Structure) ->
    read(IProto, S, undefined);

read(IProto, {list, Type}) ->
    #protocol_list_begin{etype = EType, size = Size} =
        read(IProto, list_begin),
    List = [Result || {ok, Result} <-
                          [read(IProto, Type) || _X <- lists:duplicate(Size, 0)]],
    ok = read(IProto, list_end),
    {ok, List};

read(IProto, {map, KeyType, ValType}) ->
    #protocol_map_begin{size = Size} =
        read(IProto, map_begin),

    List = [{Key, Val} || {{ok, Key}, {ok, Val}} <-
                              [{read(IProto, KeyType),
                                read(IProto, ValType)} || _X <- lists:duplicate(Size, 0)]],
    ok = read(IProto, map_end),
    {ok, dict:from_list(List)};

read(IProto, {set, Type}) ->
    #protocol_set_begin{etype = _EType,
                        size = Size} =
        read(IProto, set_begin),
    List = [Result || {ok, Result} <-
                          [read(IProto, Type) || _X <- lists:duplicate(Size, 0)]],
    ok = read(IProto, set_end),
    {ok, sets:from_list(List)};

read(#protocol{module = Module,
               data = ModuleData}, ProtocolType) ->
    Module:read(ModuleData, ProtocolType).

read_struct_loop(IProto, SDict, RTuple) ->
    #protocol_field_begin{type = FType, id = Fid, name = Name} =
        thrift_protocol:read(IProto, field_begin),
    case {FType, Fid} of
        {?tType_STOP, _} ->
            RTuple;
        _Else ->
            case dict:find(Fid, SDict) of
                {ok, {Type, Index}} ->
                    case term_to_typeid(Type) of
                        FType ->
                            {ok, Val} = read(IProto, Type),
                            thrift_protocol:read(IProto, field_end),
                            NewRTuple = setelement(Index, RTuple, Val),
                            read_struct_loop(IProto, SDict, NewRTuple);
                        Expected ->
                            error_logger:info_msg(
                              "Skipping field ~p with wrong type (~p != ~p)~n",
                              [Fid, FType, Expected]),
                            skip_field(FType, IProto, SDict, RTuple)
                    end;
                _Else2 ->
                    error_logger:info_msg("Skipping field ~p with unknown fid~n", [Fid]),
                    skip_field(FType, IProto, SDict, RTuple)
            end
    end.

skip_field(FType, IProto, SDict, RTuple) ->
    FTypeAtom = thrift_protocol:typeid_to_atom(FType),
    thrift_protocol:skip(IProto, FTypeAtom),
    read(IProto, field_end),
    read_struct_loop(IProto, SDict, RTuple).


skip(Proto, struct) ->
    ok = read(Proto, struct_begin),
    ok = skip_struct_loop(Proto),
    ok = read(Proto, struct_end);

skip(Proto, map) ->
    Map = read(Proto, map_begin),
    ok = skip_map_loop(Proto, Map),
    ok = read(Proto, map_end);

skip(Proto, set) ->
    Set = read(Proto, set_begin),
    ok = skip_set_loop(Proto, Set),
    ok = read(Proto, set_end);

skip(Proto, list) ->
    List = read(Proto, list_begin),
    ok = skip_list_loop(Proto, List),
    ok = read(Proto, list_end);

skip(Proto, Type) when is_atom(Type) ->
    _Ignore = read(Proto, Type),
    ok.


skip_struct_loop(Proto) ->
    #protocol_field_begin{type = Type} = read(Proto, field_begin),
    case Type of
        ?tType_STOP ->
            ok;
        _Else ->
            skip(Proto, Type),
            ok = read(Proto, field_end),
            skip_struct_loop(Proto)
    end.

skip_map_loop(Proto, Map = #protocol_map_begin{ktype = Ktype,
                                               vtype = Vtype,
                                               size = Size}) ->
    case Size of
        N when N > 0 ->
            skip(Proto, Ktype),
            skip(Proto, Vtype),
            skip_map_loop(Proto,
                          Map#protocol_map_begin{size = Size - 1});
        0 -> ok
    end.

skip_set_loop(Proto, Map = #protocol_set_begin{etype = Etype,
                                               size = Size}) ->
    case Size of
        N when N > 0 ->
            skip(Proto, Etype),
            skip_set_loop(Proto,
                          Map#protocol_set_begin{size = Size - 1});
        0 -> ok
    end.

skip_list_loop(Proto, Map = #protocol_list_begin{etype = Etype,
                                                 size = Size}) ->
    case Size of
        N when N > 0 ->
            skip(Proto, Etype),
            skip_list_loop(Proto,
                           Map#protocol_list_begin{size = Size - 1});
        0 -> ok
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
%%       | term()   -- for base types
%%
%% Description:
%%--------------------------------------------------------------------
write(Proto, {{struct, StructDef}, Data})
  when is_list(StructDef), is_tuple(Data), length(StructDef) == size(Data) - 1 ->

    [StructName | Elems] = tuple_to_list(Data),
    ok = write(Proto, #protocol_struct_begin{name = StructName}),
    ok = struct_write_loop(Proto, StructDef, Elems),
    ok = write(Proto, struct_end),
    ok;

write(Proto, {{struct, {Module, StructureName}}, Data})
  when is_atom(Module),
       is_atom(StructureName),
       element(1, Data) =:= StructureName ->
    StructType = Module:struct_info(StructureName),
    write(Proto, {Module:struct_info(StructureName), Data});

write(Proto, {{list, Type}, Data})
  when is_list(Data) ->
    ok = write(Proto,
               #protocol_list_begin{
                 etype = term_to_typeid(Type),
                 size = length(Data)
                }),
    lists:foreach(fun(Elem) ->
                          ok = write(Proto, {Type, Elem})
                  end,
                  Data),
    ok = write(Proto, list_end),
    ok;

write(Proto, {{map, KeyType, ValType}, Data}) ->
    ok = write(Proto,
               #protocol_map_begin{
                 ktype = term_to_typeid(KeyType),
                 vtype = term_to_typeid(ValType),
                 size  = dict:size(Data)
                }),
    dict:fold(fun(KeyData, ValData, _Acc) ->
                      ok = write(Proto, {KeyType, KeyData}),
                      ok = write(Proto, {ValType, ValData})
              end,
              _AccO = ok,
              Data),
    ok = write(Proto, map_end),
    ok;

write(Proto, {{set, Type}, Data}) ->
    true = sets:is_set(Data),
    ok = write(Proto,
               #protocol_set_begin{
                 etype = term_to_typeid(Type),
                 size  = sets:size(Data)
                }),
    sets:fold(fun(Elem, _Acc) ->
                      ok = write(Proto, {Type, Elem})
              end,
              _Acc0 = ok,
              Data),
    ok = write(Proto, set_end),
    ok;

write(#protocol{module = Module,
                data = ModuleData}, Data) ->
    Module:write(ModuleData, Data).

struct_write_loop(Proto, [{Fid, Type} | RestStructDef], [Data | RestData]) ->
    case Data of
        undefined ->
            % null fields are skipped in response
            skip;
        _ ->
            ok = write(Proto,
                       #protocol_field_begin{
                         type = term_to_typeid(Type),
                         id = Fid
                        }),
            ok = write(Proto, {Type, Data}),
            ok = write(Proto, field_end)
    end,
    struct_write_loop(Proto, RestStructDef, RestData);
struct_write_loop(Proto, [], []) ->
    ok = write(Proto, field_stop),
    ok.
