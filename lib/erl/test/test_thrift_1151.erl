-module(test_thrift_1151).

-include("thrift1151_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unmatched_struct_test() ->
  S1 = #structC{x=#structB{x=1}},
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  ?assertException (error, struct_unmatched,
    thrift_protocol:write(Protocol,
      {{struct, element(2, thrift1151_types:struct_info('structC'))}, S1})).

badarg_test() ->
  S2 = #structC{x=#structA{x="1"}},
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  ?assertException (error, badarg,
    thrift_protocol:write(Protocol,
      {{struct, element(2, thrift1151_types:struct_info('structC'))}, S2})).

-endif.
