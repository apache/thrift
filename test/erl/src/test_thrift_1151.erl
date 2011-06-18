-module(test_thrift_1151).

-include("thrift1151_types.hrl").

-export([t/0, t1/0]).

t() ->
  S1 = #structC{x=#structB{x=1}},
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  thrift_protocol:write(Protocol,
    {{struct, element(2, thrift1151_types:struct_info('structC'))}, S1}).

t1() ->
  S2 = #structC{x=#structA{x="1"}},
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  thrift_protocol:write(Protocol,
    {{struct, element(2, thrift1151_types:struct_info('structC'))}, S2}).
