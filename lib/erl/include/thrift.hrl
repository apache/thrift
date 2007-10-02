%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-define(CONFIG_FILE, filename:join("conf", "thrift.conf")).

-define(ERROR(F, D),
	error_logger:format(F, D)).

-define(INFO(Type, Report),
	error_logger:info_report({thrift_info, Type}, Report)).

-include("thrift_macros.hrl").
-include("thrift_constants.hrl").
