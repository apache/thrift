%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-define(ERROR(F, D),
	error_logger:format(F, D)).
-define(INFO(Type, Report),
	error_logger:info_report({thrift_info, Type}, Report)).

% local (same process)
-define(L0(Method), oop:call(This, Method, [])).
-define(L1(Method, Arg1), oop:call(This, Method, [Arg1])).
-define(L2(Method, Arg1, Arg2), oop:call(This, Method, [Arg1, Arg2])).
-define(L3(Method, Arg1, Arg2, Arg3), oop:call(This, Method, [Arg1, Arg2, Arg3])).
-define(L4(Method, Arg1, Arg2, Arg3, Arg4), oop:call(This, Method, [Arg1, Arg2, Arg3, Arg4])).
-define(L5(Method, Arg1, Arg2, Arg3, Arg4, Arg5), oop:call(This, Method, [Arg1, Arg2, Arg3, Arg4, Arg5])).
-define(L6(Method, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), oop:call(This, Method, [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6])).

% local (same process), but not This (e.g. t*Factory)
-define(F0(Obj, Method), oop:call(Obj, Method, [])).
-define(F1(Obj, Method, Arg1), oop:call(Obj, Method, [Arg1])).
-define(F2(Obj, Method, Arg1, Arg2), oop:call(Obj, Method, [Arg1, Arg2])).
-define(F3(Obj, Method, Arg1, Arg2, Arg3), oop:call(Obj, Method, [Arg1, Arg2, Arg3])).
-define(F4(Obj, Method, Arg1, Arg2, Arg3, Arg4), oop:call(Obj, Method, [Arg1, Arg2, Arg3, Arg4])).
-define(F5(Obj, Method, Arg1, Arg2, Arg3, Arg4, Arg5), oop:call(Obj, Method, [Arg1, Arg2, Arg3, Arg4, Arg5])).
-define(F6(Obj, Method, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), oop:call(Obj, Method, [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6])).

% remote (different process)
-define(RT0(ServerRef, Method, Timeout), gen_server:call(ServerRef, {Method, []}, Timeout)).
-define(RT1(ServerRef, Method, Timeout, Arg1), gen_server:call(ServerRef, {Method, [Arg1]}, Timeout)).
-define(RT2(ServerRef, Method, Timeout, Arg1, Arg2), gen_server:call(ServerRef, {Method, [Arg1, Arg2]}, Timeout)).
-define(RT3(ServerRef, Method, Timeout, Arg1, Arg2, Arg3), gen_server:call(ServerRef, {Method, [Arg1, Arg2, Arg3]}, Timeout)).
-define(RT4(ServerRef, Method, Timeout, Arg1, Arg2, Arg3, Arg4), gen_server:call(ServerRef, {Method, [Arg1, Arg2, Arg3, Arg4]}, Timeout)).
-define(RT5(ServerRef, Method, Timeout, Arg1, Arg2, Arg3, Arg4, Arg5), gen_server:call(ServerRef, {Method, [Arg1, Arg2, Arg3, Arg4, Arg5]}, Timeout)).
-define(RT6(ServerRef, Method, Timeout, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), gen_server:call(ServerRef, {Method, [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6]}, Timeout)).

% remote (different process), default timeout
-define(DEFAULT_TIMEOUT, 5000).
-define(R0(ServerRef, Method), ?RT0(ServerRef, Method, ?DEFAULT_TIMEOUT)).
-define(R1(ServerRef, Method, Arg1), ?RT1(ServerRef, Method, ?DEFAULT_TIMEOUT, Arg1)).
-define(R2(ServerRef, Method, Arg1, Arg2), ?RT2(ServerRef, Method, ?DEFAULT_TIMEOUT, Arg1, Arg2)).
-define(R3(ServerRef, Method, Arg1, Arg2, Arg3), ?RT3(ServerRef, Method, ?DEFAULT_TIMEOUT, Arg1, Arg2, Arg3)).
-define(R4(ServerRef, Method, Arg1, Arg2, Arg3, Arg4), ?RT4(ServerRef, Method, ?DEFAULT_TIMEOUT, Arg1, Arg2, Arg3, Arg4)).
-define(R5(ServerRef, Method, Arg1, Arg2, Arg3, Arg4, Arg5), ?RT5(ServerRef, Method, ?DEFAULT_TIMEOUT, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(R6(ServerRef, Method, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?RT6(ServerRef, Method, ?DEFAULT_TIMEOUT, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

% remote (different process), cast
-define(C0(ServerRef, Method), gen_server:cast(ServerRef, {Method, []})).
-define(C1(ServerRef, Method, Arg1), gen_server:cast(ServerRef, {Method, [Arg1]})).
-define(C2(ServerRef, Method, Arg1, Arg2), gen_server:cast(ServerRef, {Method, [Arg1, Arg2]})).
-define(C3(ServerRef, Method, Arg1, Arg2, Arg3), gen_server:cast(ServerRef, {Method, [Arg1, Arg2, Arg3]})).
-define(C4(ServerRef, Method, Arg1, Arg2, Arg3, Arg4), gen_server:cast(ServerRef, {Method, [Arg1, Arg2, Arg3, Arg4]})).
-define(C5(ServerRef, Method, Arg1, Arg2, Arg3, Arg4, Arg5), gen_server:cast(ServerRef, {Method, [Arg1, Arg2, Arg3, Arg4, Arg5]})).
-define(C6(ServerRef, Method, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), gen_server:cast(ServerRef, {Method, [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6]})).

% spawn new server
%% -define(NEW(Class, Args), 						        %%
%% 	gen_server:start_link(thrift_oop_server, {Class, Args}, [])).	        %%
%% moved to oop:start_new/2

% old
%% -define(M0(Obj, Method), oop:call(Obj, Method, [])).										     %%
%% -define(M1(Obj, Method, Arg1), oop:call(Obj, Method, [Arg1])).								     %%
%% -define(M2(Obj, Method, Arg1, Arg2), oop:call(Obj, Method, [Arg1, Arg2])).							     %%
%% -define(M3(Obj, Method, Arg1, Arg2, Arg3), oop:call(Obj, Method, [Arg1, Arg2, Arg3])).					     %%
%% -define(M4(Obj, Method, Arg1, Arg2, Arg3, Arg4), oop:call(Obj, Method, [Arg1, Arg2, Arg3, Arg4])).				     %%
%% -define(M5(Obj, Method, Arg1, Arg2, Arg3, Arg4, Arg5), oop:call(Obj, Method, [Arg1, Arg2, Arg3, Arg4, Arg5])).		     %%
%% -define(M6(Obj, Method, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), oop:call(Obj, Method, [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6])).	     %%

%%% implicit call: old

%% -define(M0(Obj, Method), ((?CLASS(Obj)):Method(Obj))).										 %%
%% -define(M1(Obj, Method, Arg1), ((?CLASS(Obj)):Method(Obj, Arg1))).									 %%
%% -define(M2(Obj, Method, Arg1, Arg2), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2))).							 %%
%% -define(M3(Obj, Method, Arg1, Arg2, Arg3), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2, Arg3))).						 %%
%% -define(M4(Obj, Method, Arg1, Arg2, Arg3, Arg4), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2, Arg3, Arg4))).				 %%
%% -define(M5(Obj, Method, Arg1, Arg2, Arg3, Arg4, Arg5), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2, Arg3, Arg4, Arg5))).			 %%
%% -define(M6(Obj, Method, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ((?CLASS(Obj)):Method(Obj, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6))).	 %%

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

% -include("tApplicationException.hrl").
