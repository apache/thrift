%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

%% TType
-define(tType_STOP, 0).
-define(tType_VOID, 1).
-define(tType_BOOL, 2).
-define(tType_BYTE, 3).
-define(tType_DOUBLE, 4).
-define(tType_I16, 6).
-define(tType_I32, 8).
-define(tType_I64, 10).
-define(tType_STRING, 11).
-define(tType_STRUCT, 12).
-define(tType_MAP, 13).
-define(tType_SET, 14).
-define(tType_LIST, 15).

% TMessageType
-define(tMessageType_CALL, 1).
-define(tMessageType_REPLY, 2).
-define(tMessageType_EXCEPTION, 3).

% TApplicationException
-define(TApplicationException_Structure,
        {struct, [{1, string},
                  {2, i32}]}).

-record('TApplicationException', {message, type}).

-define(TApplicationException_UNKNOWN, 0).
-define(TApplicationException_UNKNOWN_METHOD, 1).
-define(TApplicationException_INVALID_MESSAGE_TYPE, 2).
-define(TApplicationException_WRONG_METHOD_NAME, 3).
-define(TApplicationException_BAD_SEQUENCE_ID, 4).
-define(TApplicationException_MISSING_RESULT, 5).

