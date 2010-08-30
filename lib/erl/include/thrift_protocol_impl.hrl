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

%% Signature specifications for protocol implementations.

-ifndef(THRIFT_PROTOCOL_IMPL_INCLUDED).
-define(THRIFT_PROTOCOL_IMPL_INCLUDED, true).

-spec flush_transport(state()) -> ok.
-spec close_transport(state()) -> ok.

-spec write(state(), term()) -> ok | {error, _Reason}.

-spec read
        (state(), non_neg_integer()) ->  {ok, binary()}     | {error, _Reason};
        (state(), tprot_empty_tag()) ->   ok                | {error, _Reason};
        (state(), tprot_header_tag()) -> tprot_header_val() | {error, _Reason};
        (state(), tprot_data_tag()) ->   {ok, term()}       | {error, _Reason}.


-endif.
