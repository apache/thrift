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
-export([start_link/2, start_link/3, start_link/4, call/3, send_call/3, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-record(state, {service, protocol, seqid}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Port, Service) when is_integer(Port), is_atom(Service) ->
    start_link(Host, Port, Service, []).


%%
%% Splits client options into protocol options and transport options
%%
%% split_options([Options...]) -> {ProtocolOptions, TransportOptions}
%%
split_options(Options) ->
    split_options(Options, [], []).

split_options([], ProtoIn, TransIn) ->
    {ProtoIn, TransIn};

split_options([Opt = {OptKey, _} | Rest], ProtoIn, TransIn)
  when OptKey =:= strict_read;
       OptKey =:= strict_write ->
    split_options(Rest, [Opt | ProtoIn], TransIn);

split_options([Opt = {OptKey, _} | Rest], ProtoIn, TransIn)
  when OptKey =:= framed;
       OptKey =:= connect_timeout;
       OptKey =:= sockopts ->
    split_options(Rest, ProtoIn, [Opt | TransIn]).


%% Backwards-compatible starter for the common-case of socket transports
start_link(Host, Port, Service, Options)
  when is_integer(Port), is_atom(Service), is_list(Options) ->
    {ProtoOpts, TransOpts} = split_options(Options),

    {ok, TransportFactory} =
        thrift_socket_transport:new_transport_factory(Host, Port, TransOpts),

    {ok, ProtocolFactory} = thrift_binary_protocol:new_protocol_factory(
                              TransportFactory, ProtoOpts),

    start_link(ProtocolFactory, Service).


%% ProtocolFactory :: fun() -> thrift_protocol()
start_link(ProtocolFactory, Service)
  when is_function(ProtocolFactory), is_atom(Service) ->
    case gen_server:start_link(?MODULE, [Service], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {connect, ProtocolFactory}) of
                ok ->
                    {ok, Pid};
                Error ->
                    Error
            end;
        Else ->
            Else
    end.

call(Client, Function, Args)
  when is_pid(Client), is_atom(Function), is_list(Args) ->
    case gen_server:call(Client, {call, Function, Args}) of
        R = {ok, _} -> R;
        R = {error, _} -> R;
        {exception, Exception} -> throw(Exception)
    end.

cast(Client, Function, Args)
  when is_pid(Client), is_atom(Function), is_list(Args) ->
    gen_server:cast(Client, {call, Function, Args}).

%% Sends a function call but does not read the result. This is useful
%% if you're trying to log non-oneway function calls to write-only
%% transports like thrift_disk_log_transport.
send_call(Client, Function, Args)
  when is_pid(Client), is_atom(Function), is_list(Args) ->
    gen_server:call(Client, {send_call, Function, Args}).

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
init([Service]) ->
    {ok, #state{service = Service}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({connect, ProtocolFactory}, _From,
            State = #state{service = Service}) ->
    case ProtocolFactory() of
        {ok, Protocol} ->
            {reply, ok, State#state{protocol = Protocol,
                                    seqid = 0}};
        Error ->
            {stop, normal, Error, State}
    end;

handle_call({call, Function, Args}, _From, State = #state{service = Service}) ->
    Result = catch_function_exceptions(
               fun() ->
                       ok = send_function_call(State, Function, Args),
                       receive_function_result(State, Function)
               end,
               Service),
    {reply, Result, State};


handle_call({send_call, Function, Args}, _From, State = #state{service = Service}) ->
    Result = catch_function_exceptions(
               fun() ->
                       send_function_call(State, Function, Args)
               end,
               Service),
    {reply, Result, State}.


%% Helper function that catches exceptions thrown by sending or receiving
%% a function and returns the correct response for call or send_only above.
catch_function_exceptions(Fun, Service) ->
    try
        Fun()
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
    end.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({call, Function, Args}, State = #state{service = Service,
                                                   protocol = Protocol,
                                                   seqid = SeqId}) ->
    _Result =
        try
            ok = send_function_call(State, Function, Args),
            receive_function_result(State, Function)
        catch
            Class:Reason ->
                error_logger:error_msg("error ignored in handle_cast({cast,...},...): ~p:~p~n", [Class, Reason])
        end,

    {noreply, State};

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
terminate(Reason, State = #state{protocol=undefined}) ->
    ok;
terminate(Reason, State = #state{protocol=Protocol}) ->
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
            oneway_void) ->
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
