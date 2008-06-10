-module(thrift_protocol).

-export([new/2,
         write/2,
         read/2,
         skip/2,

         typeid_to_atom/1,

         behaviour_info/1]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-record(protocol, {module, data}).

behaviour_info(callbacks) ->
    [
     {read, 2},
     {write, 2}
    ];
behaviour_info(_Else) -> undefined.


new(Module, Data) when is_atom(Module) ->
    {ok, #protocol{module = Module,
                   data = Data}}.


write(#protocol{module = Module,
                data = ModuleData}, Data) ->
    Module:write(ModuleData, Data).

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
    
read(#protocol{module = Module,
               data = ModuleData}, ProtocolType) ->
    Module:read(ModuleData, ProtocolType).



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
