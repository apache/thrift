-module(thrift_base64_transport).

-behaviour(thrift_transport).

%% API
-export([new/1, new_transport_factory/1]).

%% thrift_transport callbacks
-export([write/2, read/2, flush/1, close/1]).

%% State
-record(b64_transport, {wrapped}).

new(Wrapped) ->
    State = #b64_transport{wrapped = Wrapped},
    thrift_transport:new(?MODULE, State).


write(#b64_transport{wrapped = Wrapped}, Data) ->
    thrift_transport:write(Wrapped, base64:encode(iolist_to_binary(Data))).


%% base64 doesn't support reading quite yet since it would involve
%% nasty buffering and such
read(#b64_transport{wrapped = Wrapped}, Data) ->
    {error, no_reads_allowed}.


flush(#b64_transport{wrapped = Wrapped}) ->
    thrift_transport:write(Wrapped, <<"\n">>),
    thrift_transport:flush(Wrapped).


close(Me = #b64_transport{wrapped = Wrapped}) ->
    flush(Me),
    thrift_transport:close(Wrapped).


%%%% FACTORY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_transport_factory(WrapFactory) ->
    F = fun() ->
                {ok, Wrapped} = WrapFactory(),
                new(Wrapped)
        end,
    {ok, F}.
