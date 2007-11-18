%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-define(tProtocolException_UNKNOWN, 0).
-define(tProtocolException_INVALID_DATA, 1).
-define(tProtocolException_NEGATIVE_SIZE, 2).
-define(tProtocolException_SIZE_LIMIT, 3).
-define(tProtocolException_BAD_VERSION, 4).

-record(tProtocolException, {super}).


