-module(thrift_transport).

-export([behaviour_info/1,

         new/2,
         write/2,
         read/2,
         flush/1
        ]).

behaviour_info(callbacks) ->
    [{write/2,
      read/2,
      flush/1}];
behaviour_info(_Else) -> undefined.


-record(transport, { module, data }).


new(Module, Data) when is_atom(Module) ->
    {ok, #transport{module = Module,
                    data = Data}}.

write(Transport, Data) when is_binary(Data) ->
    Module = Transport#transport.module,
    Module:write(Transport#transport.data, Data).

read(Transport, Len) when is_integer(Len) ->
    Module = Transport#transport.module,
    Module:read(Transport#transport.data, Len).

flush(#transport{module = Module, data = Data}) ->
    Module:flush(Data).
