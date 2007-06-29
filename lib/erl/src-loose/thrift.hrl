-define(CLASS(Obj), element(1,Obj)).

-define(M0(Obj, Method), ((?CLASS(Obj)):Method(Obj))).
-define(M1(Obj, Method, Arg1), ((?CLASS(Obj)):Method(Obj, Arg1))).
-define(M2(Obj, Method, Arg1, Arg2), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2))).
-define(M3(Obj, Method, Arg1, Arg2, Arg3), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2, Arg3))).
-define(M4(Obj, Method, Arg1, Arg2, Arg3, Arg4), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2, Arg3, Arg4))).
-define(M5(Obj, Method, Arg1, Arg2, Arg3, Arg4, Arg5), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2, Arg3, Arg4, Arg5))).
-define(M6(Obj, Method, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6))).

-define(ATTR(X), This#?MODULE.X).

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

% tmessagetype
-define(tMessageType_CALL, 1).
-define(tMessageType_REPLY, 2).
-define(tMessageType_EXCEPTION, 3).

% TProcessor
% ?

-include("thrift/tApplicationException.hrl").
