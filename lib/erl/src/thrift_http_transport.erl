%%%-------------------------------------------------------------------
%%% File    : thrift_http_transport.erl
%%% Author  : <dreiss@facebook.com>
%%% Description : Client-only HTTP-based transport for thrift
%%%
%%% Created : 24 May 2008 by <dreiss@facebook.com>
%%%-------------------------------------------------------------------
-module(thrift_http_transport).

-behaviour(gen_server).
-behaviour(thrift_transport).

%% API
-export([new/2, set_http_options/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% thrift_transport callbacks
-export([write/2, read/2, flush/1, close/1]).

-record(http_transport, {host, % string()
                         path, % string()
                         read_buffer, % iolist()
                         write_buffer, % iolist()
                         http_options
                        }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: new() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
new(Host, Path) ->
    case gen_server:start_link(?MODULE, {Host, Path}, []) of
        {ok, Pid} ->
            thrift_transport:new(?MODULE, Pid);
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% Function: set_http_options() -> ok | {error,Error}
%% Description: Set HTTP options
%%--------------------------------------------------------------------
set_http_options(Transport, HTTPOptions) ->
    gen_server:call(Transport, {set_http_options, HTTPOptions}).

%%--------------------------------------------------------------------
%% Function: write(Transport, Data) -> ok
%%
%% Data = iolist()
%%
%% Description: Writes data into the buffer
%%--------------------------------------------------------------------
write(Transport, Data) ->
    gen_server:call(Transport, {write, Data}).

%%--------------------------------------------------------------------
%% Function: flush(Transport) -> ok
%%
%% Description: Flushes the buffer, making a request
%%--------------------------------------------------------------------
flush(Transport) ->
    gen_server:call(Transport, flush).

%%--------------------------------------------------------------------
%% Function: close(Transport) -> ok
%%
%% Description: Closes the transport
%%--------------------------------------------------------------------
close(Transport) ->
    gen_server:cast(Transport, close).

%%--------------------------------------------------------------------
%% Function: Read(Transport, Len) -> {ok, Data}
%%
%% Data = binary()
%%
%% Description: Reads data through from the wrapped transoprt
%%--------------------------------------------------------------------
read(Transport, Len) when is_integer(Len) ->
    gen_server:call(Transport, {read, Len}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Host, Path}) ->
    {ok, #http_transport{host = Host,
                         path = Path,
                         read_buffer = [],
                         write_buffer = [],
                         http_options = []}}.

handle_call({write, Data}, _From, State = #http_transport{write_buffer = WBuf}) ->
    {reply, ok, State#http_transport{write_buffer = [WBuf, Data]}};

handle_call({read, Len}, _From, State = #http_transport{read_buffer = RBuf}) ->
    %% Pull off Give bytes, return them to the user, leave the rest in the buffer.
    Give = min(iolist_size(RBuf), Len),
    case iolist_to_binary(RBuf) of
        <<Data:Give/binary, RBuf1/binary>> ->
            Response = {ok, Data},
            State1 = State#http_transport{read_buffer=RBuf1},
            {reply, Response, State1};
        _ ->
            {reply, {error, 'EOF'}, State}
    end;

handle_call({set_http_options, HTTPOptions}, _From, State) ->
    {reply, ok, State#http_transport{http_options = HTTPOptions}};

handle_call(flush, _From, State) ->
    {Response, State1} = do_flush(State),
    {reply, Response, State1}.

handle_cast(close, State) ->
    {_, State1} = do_flush(State),
    {stop, normal, State1};

handle_cast(_Msg, State=#http_transport{}) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_flush(State = #http_transport{host = Host,
                                 path = Path,
                                 read_buffer = Rbuf,
                                 write_buffer = Wbuf,
                                 http_options = HttpOptions}) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
      http:request(post,
                   {"http://" ++ Host ++ Path,
                    [{"User-Agent", "Erlang/thrift_http_transport"}],
                    "application/x-thrift",
                    iolist_to_binary(Wbuf)},
                   HttpOptions,
                   [{body_format, binary}]),

    State1 = State#http_transport{read_buffer = [Rbuf, Body],
                                  write_buffer = []},
    {ok, State1}.

min(A,B) when A<B -> A;
min(_,B)          -> B.
