-module(thrift_file_transport).
-author(todd@amiestreet.com).

-behaviour(thrift_transport).

-export([new_reader/1,
         new/1,
         new/2,
         write/2, read/2, flush/1, close/1]).

-record(t_file_transport, {device,
                           should_close = true,
                           mode = write}).

%%%% CONSTRUCTION   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_reader(Filename) ->
    case file:open(Filename, [read, binary, {read_ahead, 1024*1024}]) of
        {ok, IODevice} ->
            new(IODevice, [{should_close, true}, {mode, read}]);
        Error -> Error
    end.

new(Device) ->
    new(Device, []).

%% Device :: io_device()
%%
%% Device should be opened in raw and binary mode.
new(Device, Opts) when is_list(Opts) ->
    State = parse_opts(Opts, #t_file_transport{device = Device}),
    thrift_transport:new(?MODULE, State).


%% Parse options
parse_opts([{should_close, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_opts(Rest, State#t_file_transport{should_close = Bool});
parse_opts([{mode, Mode} | Rest], State)
  when Mode =:= write;
       Mode =:= read ->
    parse_opts(Rest, State#t_file_transport{mode = Mode});
parse_opts([], State) ->
     State.


%%%% TRANSPORT IMPL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write(#t_file_transport{device = Device, mode = write}, Data) ->
    file:write(Device, Data);
write(_T, _D) ->
    {error, read_mode}.


read(#t_file_transport{device = Device, mode = read}, Len)
  when is_integer(Len), Len >= 0 ->
    file:read(Device, Len);
read(_T, _D) ->
    {error, read_mode}.

flush(#t_file_transport{device = Device, mode = write}) ->
    file:sync(Device).

close(#t_file_transport{device = Device, should_close = SC}) ->
    case SC of
        true ->
            file:close(Device);
        false ->
            ok
    end.
