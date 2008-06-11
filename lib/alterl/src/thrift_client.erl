%%%-------------------------------------------------------------------
%%% File    : thrift_client.erl
%%% Author  : Todd Lipcon <todd@lipcon.org>
%%% Description : A client which connects to a thrift service
%%%
%%% Created : 24 Feb 2008 by Todd Lipcon <todd@lipcon.org>
%%%-------------------------------------------------------------------
-module(thrift_client).

-behaviour(gen_server).

%% API
-export([start_link/3, start_link/4, call/3, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-record(state, {service, protocol, seqid,
                strict_read = true,
                strict_write = true,
                framed = false,
                connect_timeout = infinity
               }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Port, Service) ->
    start_link(Host, Port, Service, []).

start_link(Host, Port, Service, Options) when is_integer(Port), is_atom(Service), is_list(Options) ->
    gen_server:start_link(?MODULE, [Host, Port, Service, Options], []).

call(Client, Function, Args)
  when is_pid(Client), is_atom(Function), is_list(Args) ->
    case gen_server:call(Client, {call, Function, Args}) of
        R = {ok, _} -> R;
        R = {error, _} -> R;
        {exception, Exception} -> throw(Exception)
    end.

close(Client) when is_pid(Client) ->
    gen_server:cast(Client, close).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Port, Service, Options]) ->
    State = parse_options(Options, #state{}),

    TcpOptions = [binary,
                  {packet, 0},
                  {active, false},
                  {nodelay, true}],
    TcpTimeout = State#state.connect_timeout,

    case gen_tcp:connect(Host, Port, TcpOptions, TcpTimeout) of
        {ok, Sock} ->
            {ok, Transport} = thrift_socket_transport:new(Sock),
            {ok, BufTransport} =
                case State#state.framed of
                    true  -> thrift_framed_transport:new(Transport);
                    false -> thrift_buffered_transport:new(Transport)
                end,
            {ok, Protocol} = thrift_binary_protocol:new(BufTransport,
                                 [{strict_read,  State#state.strict_read},
                                  {strict_write, State#state.strict_write}]),

            {ok, State#state{service  = Service,
                             protocol = Protocol,
                             seqid    = 0}};
        Error ->
            {stop, Error}
    end.

parse_options([], State) ->
    State;
parse_options([{strict_read, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_options(Rest, State#state{strict_read=Bool});
parse_options([{strict_write, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_options(Rest, State#state{strict_write=Bool});
parse_options([{framed, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_options(Rest, State#state{framed=Bool});
parse_options([{connect_timeout, TO} | Rest], State) when TO =:= infinity; is_integer(TO) ->
    parse_options(Rest, State#state{connect_timeout=TO}).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({call, Function, Args}, _From, State = #state{service = Service,
                                                          protocol = Protocol,
                                                          seqid = SeqId}) ->
    Result =
        try
            ok = send_function_call(State, Function, Args),
            receive_function_result(State, Function)
        catch
            throw:{return, Return} ->
                Return;
            error:function_clause ->
                ST = erlang:get_stacktrace(),
                case hd(ST) of
                    {Service, function_info, [Function, _]} ->
                        {error, {no_function, Function}};
                    _ -> throw({error, {function_clause, ST}})
                end
        end,

    {reply, Result, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(close, State=#state{protocol = Protocol}) ->
%%     error_logger:info_msg("thrift_client ~p received close", [self()]),
    {stop,normal,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State = #state{protocol = Protocol}) ->
%%     error_logger:info_msg("thrift_client ~p terminating due to ~p", [self(), Reason]),
    thrift_protocol:close_transport(Protocol),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_function_call(#state{protocol = Proto,
                          service  = Service,
                          seqid    = SeqId},
                   Function,
                   Args) ->
    Params = Service:function_info(Function, params_type),
    {struct, PList} = Params,
    if
        length(PList) =/= length(Args) ->
            throw({return, {error, {bad_args, Function, Args}}});
        true -> ok
    end,

    Begin = #protocol_message_begin{name = atom_to_list(Function),
                                    type = ?tMessageType_CALL,
                                    seqid = SeqId},
    ok = thrift_protocol:write(Proto, Begin),
    ok = thrift_protocol:write(Proto, {Params, list_to_tuple([Function | Args])}),
    ok = thrift_protocol:write(Proto, message_end),
    thrift_protocol:flush_transport(Proto),
    ok.

receive_function_result(State = #state{protocol = Proto,
                                       service = Service},
                        Function) ->
    ResultType = Service:function_info(Function, reply_type),
    read_result(State, Function, ResultType).

read_result(_State,
            _Function,
            async_void) ->
    {ok, ok};

read_result(State = #state{protocol = Proto,
                           seqid    = SeqId},
            Function,
            ReplyType) ->
    case thrift_protocol:read(Proto, message_begin) of
        #protocol_message_begin{seqid = RetSeqId} when RetSeqId =/= SeqId ->
            {error, {bad_seq_id, SeqId}};

        #protocol_message_begin{type = ?tMessageType_EXCEPTION} ->
            handle_application_exception(State);

        #protocol_message_begin{type = ?tMessageType_REPLY} ->
            handle_reply(State, Function, ReplyType)
    end.

handle_reply(State = #state{protocol = Proto,
                            service = Service},
             Function,
             ReplyType) ->
    {struct, ExceptionFields} = Service:function_info(Function, exceptions),
    ReplyStructDef = {struct, [{0, ReplyType}] ++ ExceptionFields},
    {ok, Reply} = thrift_protocol:read(Proto, ReplyStructDef),
    ReplyList = tuple_to_list(Reply),
    true = length(ReplyList) == length(ExceptionFields) + 1,
    ExceptionVals = tl(ReplyList),
    Thrown = [X || X <- ExceptionVals,
                   X =/= undefined],
    Result =
        case Thrown of
            [] when ReplyType == {struct, []} ->
                {ok, ok};
            [] ->
                {ok, hd(ReplyList)};
            [Exception] ->
                {exception, Exception}
        end,
    ok = thrift_protocol:read(Proto, message_end),
    Result.

handle_application_exception(State = #state{protocol = Proto}) ->
    {ok, Exception} = thrift_protocol:read(Proto,
                                           ?TApplicationException_Structure),
    ok = thrift_protocol:read(Proto, message_end),
    XRecord = list_to_tuple(
                ['TApplicationException' | tuple_to_list(Exception)]),
    error_logger:error_msg("X: ~p~n", [XRecord]),
    true = is_record(XRecord, 'TApplicationException'),
    {exception, XRecord}.
