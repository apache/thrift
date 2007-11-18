%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift_sup).

-behaviour(supervisor).

-include("thrift.hrl").

-export([start_link/3, init/1, thrift_start_link/7]).

-define(SERVER, ?MODULE).

start_link(Port, Handler, Processor) ->
    Args = [Port, Handler, Processor],
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init([Port, Handler, Processor]) ->
    TF = tBufferedTransportFactory,
    PF = tBinaryProtocolFactory,
    ST = tErlAcceptor,
    SF = tErlServer,

    ThriftModules = [TF, PF, ST, SF],

    Args = [SF, Port, Handler, Processor, ST, TF, PF],

    ThriftServer = {thrift_server, {?MODULE, thrift_start_link, Args},
                    permanent, 2000, worker, ThriftModules},

    {ok, {{one_for_one, 10, 1}, [ThriftServer]}}.

thrift_start_link(SF = tErlServer, Port, Hnd, Pr, ST, TF, PF) ->
    Args = [Port, Hnd, Pr, ST, TF:new(), PF:new()],
    Pid = oop:start_new(SF, Args),
    ?R0(Pid, effectful_serve),
    {ok, Pid}.
