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
      {{struct, element(2, thrift1151_types:struct_info('StructC'))}, S1}
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
      {{struct, element(2, thrift1151_types:struct_info('StructC'))}, S2}
    )
  ).

-endif.
