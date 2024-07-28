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

-module(validation_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("gen-erlang/thrift_test_thrift.hrl").

write_test_() ->
    [
        ?_assertMatch(ok, write(byte, +42)),
        ?_assertMatch(ok, write(byte, -42)),
        ?_assertMatch({error, {invalid, [], _}}, write(byte, -200)),
        ?_assertMatch(ok, write(string, <<"1337">>)),
        ?_assertMatch({error, {invalid, [], _}}, write(string, "1337")),
        ?_assertMatch({error, {invalid, [], _}}, write(string, 1337)),
        ?_assertMatch(
            ok,
            write({struct, struct, {thrift_test_thrift, 'EmptyStruct'}}, #'EmptyStruct'{})
        ),
        ?_assertMatch(
            {error, {invalid, [], _}},
            write({struct, struct, {thrift_test_thrift, 'EmptyStruct'}}, #'Bools'{})
        ),
        ?_assertMatch(
            ok,
            write({struct, struct, {thrift_test_thrift, 'OneField'}}, #'OneField'{
                field = #'EmptyStruct'{}
            })
        ),
        ?_assertMatch(
            {error, {invalid, [field], _}},
            write({struct, struct, {thrift_test_thrift, 'OneField'}}, #'OneField'{
                field = #'Bools'{}
            })
        )
    ].

write(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(),
    {ok, Proto} = thrift_binary_protocol:new(Trans, [{strict_read, true}, {strict_write, true}]),
    {_Proto, Result} = thrift_protocol:write(Proto, {Type, Data}),
    Result.
