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

-module(thrift_http_transport).

-behaviour(thrift_transport).

-include("thrift_constants.hrl").

%% API
-export([new/2, new/3]).

%% thrift_transport callbacks
-export([write/2, read/2, flush/1, close/1]).

-ifdef(TEST).
-export([takes_max_body_size/1]).
-endif.

% string()
-record(http_transport, {
    scheme :: http | https,
    host :: string(),
    path :: string(),
    read_buffer :: iodata(),
    write_buffer :: iodata(),
    % see httpc(3)
    http_options :: [{atom(), term()}],
    extra_headers :: [{string(), string()}],
    % undefined: the thrift application's max_message_size
    max_message_size :: pos_integer() | undefined
}).

new(Host, Path) ->
    new(Host, Path, _Options = []).

%%--------------------------------------------------------------------
%% Options include:
%%   {scheme, http | https}  = http unless given. For https, the server's
%%                             certificate and host name are checked against
%%                             the system's trusted roots; TLS options of the
%%                             caller's own go into {ssl, _} in HttpOptions
%%                             and take the place of the matching defaults
%%   {http_options, HttpOptions}  = See http(3)
%%   {extra_headers, ExtraHeaders}  = List of extra HTTP headers
%%   {max_message_size, Bytes}  = The longest reply to read, in place of the
%%                                thrift application's max_message_size
%%--------------------------------------------------------------------
new(Host, Path, Options) ->
    State1 = #http_transport{
        scheme = http,
        host = Host,
        path = Path,
        read_buffer = [],
        write_buffer = [],
        http_options = [],
        extra_headers = [],
        max_message_size = undefined
    },
    ApplyOption =
        fun
            ({scheme, Scheme}, State = #http_transport{}) when
                Scheme =:= http; Scheme =:= https
            ->
                State#http_transport{scheme = Scheme};
            ({http_options, HttpOpts}, State = #http_transport{}) ->
                State#http_transport{http_options = HttpOpts};
            ({extra_headers, ExtraHeaders}, State = #http_transport{}) ->
                State#http_transport{extra_headers = ExtraHeaders};
            ({max_message_size, Max}, State = #http_transport{}) when
                is_integer(Max), Max > 0
            ->
                State#http_transport{max_message_size = Max};
            (Other, #http_transport{}) ->
                {invalid_option, Other};
            (_, Error) ->
                Error
        end,
    case lists:foldl(ApplyOption, State1, Options) of
        State2 = #http_transport{} ->
            thrift_transport:new(?MODULE, State2);
        Else ->
            {error, Else}
    end.

%% Writes data into the buffer
write(State = #http_transport{write_buffer = WBuf}, Data) ->
    {State#http_transport{write_buffer = [WBuf, Data]}, ok}.

%% Flushes the buffer, making a request
flush(State = #http_transport{read_buffer = Rbuf, write_buffer = Wbuf}) ->
    case iolist_to_binary(Wbuf) of
        <<>> ->
            %% Don't bother flushing empty buffers.
            {State, ok};
        WBinary ->
            Result = request(State, WBinary),
            %% The request has been dealt with either way; what was written
            %% for it is not sent again with the next one.
            State1 = State#http_transport{write_buffer = []},
            case Result of
                {ok, Body} ->
                    {State1#http_transport{read_buffer = [Rbuf, Body]}, ok};
                {error, Reason} ->
                    {State1, {error, Reason}}
            end
    end.

%% httpc hands a reply over as it arrives only to an asynchronous request,
%% and each asynchronous request leaves an alias behind in the process that
%% made it. So the request is made from a process of its own, which also
%% takes along whatever is still on its way of a reply given up on.
request(
    State = #http_transport{
        scheme = Scheme,
        host = Host,
        path = Path,
        http_options = HttpOptions,
        extra_headers = ExtraHeaders
    },
    Body
) ->
    Request = {
        atom_to_list(Scheme) ++ "://" ++ Host ++ Path,
        [{"User-Agent", "Erlang/thrift_http_transport"} | ExtraHeaders],
        "application/x-thrift",
        Body
    },
    RequestHttpOptions = http_options(Scheme, HttpOptions),
    Max = max_message_size(State),
    Caller = self(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        Caller ! {self(), try_request(Request, RequestHttpOptions, Max)}
    end),
    receive
        {Pid, {result, Result}} ->
            erlang:demonitor(Monitor, [flush]),
            Result;
        {Pid, {raise, Class, Reason, Stacktrace}} ->
            erlang:demonitor(Monitor, [flush]),
            erlang:raise(Class, Reason, Stacktrace);
        {'DOWN', Monitor, process, Pid, Reason} ->
            {error, Reason}
    end.

%% An exception is raised again in the caller, as if it had made the request.
try_request(Request, HttpOptions, Max) ->
    try send_request(Request, HttpOptions, Max) of
        Result -> {result, Result}
    catch
        Class:Reason:Stacktrace -> {raise, Class, Reason, Stacktrace}
    end.

send_request(Request, HttpOptions, Max) ->
    Options = [{sync, false}, {stream, {self, once}}, {body_format, binary}],
    case httpc:request(post, Request, HttpOptions, Options ++ max_body_size_option(Max)) of
        {ok, RequestId} ->
            receive_reply(RequestId, Max);
        {error, Reason} ->
            {error, Reason}
    end.

%% httpc streams a 200 reply, and a 206 one, which it does not tell apart.
%% It hands over any other reply whole.
receive_reply(RequestId, Max) ->
    receive
        {http, {RequestId, stream_start, Headers, Handler}} ->
            case declared_length(Headers) of
                Length when is_integer(Length), Length > Max ->
                    give_up(RequestId, Max);
                _ ->
                    receive_body(RequestId, Handler, Max, [], 0)
            end;
        {http, {RequestId, {{_Version, Status, ReasonPhrase}, _Headers, _Body}}} ->
            {error, {http_status, Status, ReasonPhrase}};
        {http, {RequestId, {error, Reason}}} ->
            error_reply(Reason, Max)
    end.

%% httpc reads the next part of the body only once asked to, so no more
%% than one part arrives past the limit.
receive_body(RequestId, Handler, Max, Parts, Size) ->
    ok = httpc:stream_next(Handler),
    receive
        {http, {RequestId, stream, Part}} when Size + byte_size(Part) > Max ->
            give_up(RequestId, Max);
        {http, {RequestId, stream, Part}} ->
            receive_body(RequestId, Handler, Max, [Parts, Part], Size + byte_size(Part));
        {http, {RequestId, stream_end, _Headers}} ->
            {ok, iolist_to_binary(Parts)};
        {http, {RequestId, {error, Reason}}} ->
            error_reply(Reason, Max)
    end.

give_up(RequestId, Max) ->
    ok = httpc:cancel_request(RequestId),
    {error, {message_size_exceeds_maximum, Max}}.

%% httpc refuses a reply longer than max_body_size with one of these.
error_reply(body_too_big, Max) ->
    {error, {message_size_exceeds_maximum, Max}};
error_reply({body_too_long, _}, Max) ->
    {error, {message_size_exceeds_maximum, Max}};
error_reply(Reason, _Max) ->
    {error, Reason}.

declared_length(Headers) ->
    case lists:keyfind("content-length", 1, Headers) of
        {_, Value} ->
            try
                list_to_integer(Value)
            catch
                error:badarg -> undefined
            end;
        false ->
            undefined
    end.

%% httpc checks a server's certificate by default only from OTP 26 on, and
%% only when it is given no TLS options at all. So for https the caller's
%% TLS options go after defaults that have the certificate and the host name
%% checked; ssl:connect/3 honours the last occurrence of a duplicated option,
%% so the caller's win. The defaults are those of
%% httpc:ssl_verify_host_options(true), with the roots handled as in
%% thrift_sslsocket_transport.
http_options(http, HttpOptions) ->
    HttpOptions;
http_options(https, HttpOptions) ->
    TlsOptions = proplists:get_value(ssl, HttpOptions, []),
    Defaults =
        [{verify, verify_peer} | system_cacerts(TlsOptions)] ++
            [
                {customize_hostname_check, [
                    {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                ]}
            ],
    lists:keystore(ssl, 1, HttpOptions, {ssl, Defaults ++ TlsOptions}).

%% The system's trusted roots, unless the caller names roots itself:
%% {cacerts, _} would win over a {cacertfile, _} of the caller's.
system_cacerts(TlsOptions) ->
    case lists:any(fun is_ca_option/1, TlsOptions) of
        true ->
            [];
        false ->
            %% public_key:cacerts_get/0 raises on a host without a trust
            %% store. Leave the roots out then, and let ssl refuse the
            %% certificate, rather than fail here or not check it at all.
            try public_key:cacerts_get() of
                CaCerts -> [{cacerts, CaCerts}]
            catch
                _:_ -> []
            end
    end.

is_ca_option({cacerts, _}) -> true;
is_ca_option({cacertfile, _}) -> true;
is_ca_option(_) -> false.

max_message_size(#http_transport{max_message_size = undefined}) ->
    application:get_env(thrift, max_message_size, ?DEFAULT_MAX_MESSAGE_SIZE);
max_message_size(#http_transport{max_message_size = Max}) ->
    Max.

%% max_body_size bounds the replies httpc does not stream as well. An httpc
%% that does not take it reports it as an invalid option on every request.
max_body_size_option(Max) when is_integer(Max), Max > 0 ->
    case application:get_key(inets, vsn) of
        {ok, Vsn} ->
            case takes_max_body_size(Vsn) of
                true -> [{max_body_size, Max}];
                false -> []
            end;
        undefined ->
            []
    end;
max_body_size_option(_) ->
    [].

%% httpc has taken max_body_size since inets 9.3.2.7, 9.6.2.3 and 9.7.2, in
%% the OTP 27.3.4.17, 28.5.0.6 and 29.0.6 patches. OTP 26 does not have it.
takes_max_body_size(Vsn) ->
    try [list_to_integer(Part) || Part <- string:split(Vsn, ".", all)] of
        V ->
            V >= [9, 7, 2] orelse
                (V >= [9, 6, 2, 3] andalso V < [9, 6, 3]) orelse
                (V >= [9, 3, 2, 7] andalso V < [9, 3, 3])
    catch
        error:badarg -> false
    end.

close(State) ->
    {State, ok}.

read(State = #http_transport{read_buffer = RBuf}, Len) when is_integer(Len) ->
    %% Pull off Give bytes, return them to the user, leave the rest in the buffer.
    Give = min(iolist_size(RBuf), Len),
    case iolist_to_binary(RBuf) of
        <<Data:Give/binary, RBuf1/binary>> ->
            Response = {ok, Data},
            State1 = State#http_transport{read_buffer = RBuf1},
            {State1, Response};
        _ ->
            {State, {error, 'EOF'}}
    end.
