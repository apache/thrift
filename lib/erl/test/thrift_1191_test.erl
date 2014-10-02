-module(thrift_1191_test).
-compile(export_all).

-include("gen-erl/thrift1191_a_types.hrl").
-include_lib("eunit/include/eunit.hrl").

test_data() -> #'TheTest'{field1=256}.

thrift_1191_unset_field_test_() ->
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  {P, ok} =  thrift_protocol:write(
    Protocol,
    {
      thrift1191_a_types:struct_info('TheTest'),
      test_data()
    }
  ),
  {_, {ok, Result}} = thrift_protocol:read(
    P,
    thrift1191_a_types:struct_info('TheTest'),
    'TheTest'
  ),
  [
    {"thrift 1191 unset field test", ?_assertEqual({'TheTest',256,undefined}, Result)}
  ].

thrift_1191_removed_field_test_() ->
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol} = thrift_binary_protocol:new(Transport),
  {P, ok} =  thrift_protocol:write(
    Protocol,
    {
      thrift1191_a_types:struct_info('TheTest'),
      test_data()
    }
  ),
  {_, {ok, Result}} = thrift_protocol:read(
    P,
    thrift1191_b_types:struct_info('TheTest'),
    'TheTest'
  ),
  [
    {"thrift 1191 removed field test", ?_assertEqual({'TheTest',256}, Result)}
  ].