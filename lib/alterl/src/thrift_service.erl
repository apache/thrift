-module(thrift_service).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{service_info, 1}].
