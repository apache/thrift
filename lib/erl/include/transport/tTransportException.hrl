%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-define(tTransportException_UNKNOWN, 0).
-define(tTransportException_NOT_OPEN, 1).
-define(tTransportException_ALREADY_OPEN, 2).
-define(tTransportException_TIMED_OUT, 3).
-define(tTransportException_END_OF_FILE, 4).

-record(tTransportException, {super, type}).
