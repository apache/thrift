-module(test_thrift_1151).

-include("gen-erlang/thrift1151_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unmatched_struct_test() ->
  S1 = #'StructC'{x=#'StructB'{x=1}},
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  ?assertEqual(
    {Protocol, {error, {invalid, [x], #'StructB'{x=1}}}},
    thrift_protocol:write(
      Protocol,
      {{struct, struct, {thrift1151_types, 'StructC'}}, S1}
    )
  ).

badarg_test() ->
  S2 = #'StructC'{x=#'StructA'{x="1"}},
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  ?assertEqual(
    {Protocol, {error, {invalid, [x, x], "1"}}},
    thrift_protocol:write(
      Protocol,
      {{struct, struct, {thrift1151_types, 'StructC'}}, S2}
    )
  ).

union_test() ->
  S1 = {a, #'StructA'{x = 1}},
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  ?assertMatch(
    {_Protocol, ok},
    thrift_protocol:write(
      Protocol,
      {{struct, union, {thrift1151_types, 'UnionA'}}, S1}
    )
  ).

union_badarg_test() ->
  S1 = {a, S2 = #'StructB'{x = 42}},
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  ?assertEqual(
    {Protocol, {error, {invalid, [a], S2}}},
    thrift_protocol:write(
      Protocol,
      {{struct, union, {thrift1151_types, 'UnionA'}}, S1}
    )
  ).

-endif.
