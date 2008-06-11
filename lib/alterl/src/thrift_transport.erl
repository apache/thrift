-module(thrift_transport).

-export([behaviour_info/1]).

-export([new/2,
         write/2,
         read/2,
         flush/1,
         close/1
        ]).

behaviour_info(callbacks) ->
    [{read, 2},
     {write, 2},
     {flush, 1},
     {close, 1}
    ].

-record(transport, {module, data}).

new(Module, Data) when is_atom(Module) ->
    {ok, #transport{module = Module,
                    data = Data}}.

%% Data :: iolist()
write(Transport, Data) ->
    Module = Transport#transport.module,
    Module:write(Transport#transport.data, Data).

read(Transport, Len) when is_integer(Len) ->
    Module = Transport#transport.module,
    Module:read(Transport#transport.data, Len).

flush(#transport{module = Module, data = Data}) ->
    Module:flush(Data).

close(#transport{module = Module, data = Data}) ->
    Module:close(Data).
