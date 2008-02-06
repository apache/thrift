%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-define(VERSION_MASK, 16#FFFF0000).
-define(VERSION_1, 16#80010000).

-record(tBinaryProtocol, {super}).
