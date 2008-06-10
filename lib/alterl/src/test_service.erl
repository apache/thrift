-module(test_service).
%
% Test service definition

-export([function_info/2]).

function_info(add, params_type) ->
    {struct, [{1, i32},
              {2, i32}]};
function_info(add, reply_type) -> i32.
