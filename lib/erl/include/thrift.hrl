%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-define(ERROR(F, D),
	error_logger:format(F, D)).

-define(INFO(F, D),
	error_logger:info_msg(F, D)).

-include("thrift_macros.hrl").
-include("thrift_constants.hrl").
