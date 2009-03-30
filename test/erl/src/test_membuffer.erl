-module(test_membuffer).
-export([t/0]).

-include("thriftTest_types.hrl").

test_data() ->
    #xtruct{string_thing = <<"foobar">>,
	    byte_thing = 123,
	    i32_thing = 1234567,
	    i64_thing = 12345678900}.

t1() ->
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    TestData = test_data(),
    ok = thrift_protocol:write(Protocol,
			       {{struct, element(2, thriftTest_types:struct_info('xtruct'))},
				TestData}),
    {ok, Result} = thrift_protocol:read(Protocol,
					{struct, element(2, thriftTest_types:struct_info('xtruct'))},
					'xtruct'),

    Result = TestData.


t2() ->
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    TestData = test_data(),
    ok = thrift_protocol:write(Protocol,
			       {{struct, element(2, thriftTest_types:struct_info('xtruct'))},
				TestData}),
    {ok, Result} = thrift_protocol:read(Protocol,
					{struct, element(2, thriftTest_types:struct_info('xtruct3'))},
					'xtruct3'),

    Result = #xtruct3{string_thing = TestData#xtruct.string_thing,
                      changed = undefined,
                      i32_thing = TestData#xtruct.i32_thing,
                      i64_thing = TestData#xtruct.i64_thing}.


t() ->
    t1(),
    t2().

