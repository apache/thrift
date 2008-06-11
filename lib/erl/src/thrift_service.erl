-module(thrift_service).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{function_info, 2}].
