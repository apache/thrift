-module (thrift_socket_server_test).

-include_lib("eunit/include/eunit.hrl").

-include ("thrift_constants.hrl").

parse_handler_options_test_() ->
	CorrectServiceHandlerOptionList = [{?MULTIPLEXED_ERROR_HANDLER_KEY, ?MODULE}, {"Service1", ?MODULE}, {"Service2", ?MODULE}],
	MissingErrorHandlerOptionList   = [{"Service1", ?MODULE}, {"Service2", ?MODULE}],
	WrongService2HandlerOptionList  = [{?MULTIPLEXED_ERROR_HANDLER_KEY, ?MODULE}, {"Service1", ?MODULE}, {"Service2", "Module"}],
	WrongServiceKeyOptionList       = [{?MULTIPLEXED_ERROR_HANDLER_KEY, ?MODULE}, {service, ?MODULE}, {"Service2", ?MODULE}],
	CorrectHandlerTestFunction = fun() ->
		?assertMatch({thrift_socket_server,_,_,_,_,_,_,_,_,_,_}, thrift_socket_server:parse_options([{handler, CorrectServiceHandlerOptionList}])),
		{thrift_socket_server,_,_, HandlerList,_,_,_,_,_,_,_} = thrift_socket_server:parse_options([{handler, CorrectServiceHandlerOptionList}]),
		lists:foreach(fun
			({ServiceName, HandlerModule}) ->
				?assertMatch({ok, HandlerModule}, orddict:find(ServiceName, HandlerList))
		end, CorrectServiceHandlerOptionList)
	end,
	[
	 ?_assertThrow(_, thrift_socket_server:parse_options([{handler, []}])),
	 ?_assertThrow(_, thrift_socket_server:parse_options([{handler, ?MODULE}, {handler, CorrectServiceHandlerOptionList}])),
	 ?_assertMatch({thrift_socket_server,_,_,?MODULE,_,_,_,_,_,_,_}, thrift_socket_server:parse_options([{handler, ?MODULE}])),
	 ?_assertThrow(_, thrift_socket_server:parse_options([{handler, MissingErrorHandlerOptionList}])),
	 ?_assertThrow(_, thrift_socket_server:parse_options([{handler, WrongService2HandlerOptionList}])),
	 ?_assertThrow(_, thrift_socket_server:parse_options([{handler, WrongServiceKeyOptionList}])),
	 CorrectHandlerTestFunction
	].

parse_service_options_test_() ->
	CorrectServiceModuleOptionList = [{"Service1", ?MODULE}, {"Service2", ?MODULE}],
	WrongService2ModuleOptionList  = [{"Service1", ?MODULE}, {"Service2", "thrift_service_module"}],
	WrongServiceKeyOptionList       = [{service, ?MODULE}, {"Service2", ?MODULE}],
	CorrectServiceModuleTestFunction = fun() ->
		?assertMatch({thrift_socket_server,_,_,_,_,_,_,_,_,_,_}, thrift_socket_server:parse_options([{service, CorrectServiceModuleOptionList}])),
		{thrift_socket_server,_, ServiceModuleList,_,_,_,_,_,_,_,_} = thrift_socket_server:parse_options([{service, CorrectServiceModuleOptionList}]),
		lists:foreach(fun
			({ServiceName, ServiceModule}) ->
				?assertMatch({ok, ServiceModule}, orddict:find(ServiceName, ServiceModuleList))
		end, CorrectServiceModuleOptionList)
	end,
	[
	 {"Bad argument for the service option", ?_assertThrow(_, thrift_socket_server:parse_options([{service, []}]))},
	 {"Try to configure the service option twice", ?_assertThrow(_, thrift_socket_server:parse_options([{service, ?MODULE}, {service, CorrectServiceModuleOptionList}]))},
	 {"Set a service module for a non multiplexed service", ?_assertMatch({thrift_socket_server,_,?MODULE,_,_,_,_,_,_,_,_}, thrift_socket_server:parse_options([{service, ?MODULE}]))},
	 {"...", ?_assertThrow(_, thrift_socket_server:parse_options([{service, WrongService2ModuleOptionList}]))},
	 ?_assertThrow(_, thrift_socket_server:parse_options([{service, WrongServiceKeyOptionList}])),
	 CorrectServiceModuleTestFunction
	].
