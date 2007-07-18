%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(oop).

-export([get/2, set/3, call/2, call/3, inspect/1, start_new/2]).
-export([behaviour_info/1]).

-include("oop.hrl").

%%%
%%% behavior definition
%%%

behaviour_info(callbacks) -> 
    [ 
      {attr, 4}, 
      {super, 0} 
    ];
behaviour_info(_) -> 
    undefined.

%%

-define(TRIED, lists:reverse([TryModule|TriedRev])).

%% no super attr defined
-define(NOSUPEROBJ, exit({missing_attr_super, {inspect(Obj), ?TRIED}})).

-define(NOMETHOD, exit({missing_method, {Method, inspect(Obj), tl(Args), ?TRIED}})).

-define(NOATTR, exit({missing_attr, {hd(tl(Args)), inspect(FirstObj), ?TRIED}})).

-define(NOATTR_SET, exit({missing_attr, {Field, inspect(Obj), ".." %% TODO: give a backtrace
			}})).


%%% get(Obj, Field) -> term()
%%% looks up Field in Obj or its ancestor objects

get(Obj, Field) ->
    call(Obj, attr, [get, Field, get]).

set(Obj, Field, Value) -> %% TODO: could be tail-recursive
    Module = ?CLASS(Obj),
    try
	Module:attr(Obj, set, Field, Value)
    catch
	error:Kind when Kind == undef; Kind == function_clause ->
	    case get_superobject(Obj) of
		{ ok, Superobj } ->
		    Super1 = set(Superobj, Field, Value),
		    try
			Module:attr(Obj, set, super, Super1)
		    catch %% TODO(cpiro): remove check
			X -> exit({burnsauce, X})
		    end;
		none ->
		    ?NOATTR_SET
	    end
    end.
    

%%% C++                   <-> Erlang
%%% classes                   modules
%%%   class b : public a        a:super() -> b.
%%%   

get_superobject(Obj) ->
    try
	{ok, (?CLASS(Obj)):attr(Obj, get, super, get)}
    catch
	error:Kind when Kind == undef; Kind == function_clause ->
	    none
    end.

call(Obj, Method, ArgsProper) ->
    %% io:format("call called: Obj=~p Method=~p ArgsProper=~p~n", [oop:inspect(Obj), Method, ArgsProper]),
    Args = [Obj|ArgsProper], %% prepend This to args
    TryModule = ?CLASS(Obj),
    call_loop(Obj, Method, Args, TryModule, [], Obj).

call(Obj, Method) -> 
    call(Obj, Method, []).

call_loop(Obj, Method, Args, TryModule, TriedRev, FirstObj) ->
    try
	%% io:format("call_loop~n ~p~n ~p~n ~p~n ~p~n ~n", [Obj, Method, Args, TryModule]),
	apply(TryModule, Method, Args)
    catch
	error:Kind when Kind == undef; Kind == function_clause ->
	    case { TryModule:super(), Method } of 
		{ none, attr } ->
		    ?NOATTR;
		
		{ none, _ } -> 
		    ?NOMETHOD;
		
		{ Superclass, attr } -> 
		    %% look for attrs in the "super object"
		    
		    case get_superobject(Obj) of
			{ok, Superobj} when (TryModule == ?CLASS(Obj)) -> 
			    %% replace This with Superobj
			    NewArgs = [Superobj|tl(Args)], 
			    call_loop(Superobj, Method, NewArgs, 
				      Superclass, [TryModule|TriedRev], FirstObj);
			
			{ok, _Superobj} -> % failed guard TODO(cpiro): removeme
			    exit(oh_noes);
			
			none    -> ?NOSUPEROBJ
		    end;
		
		{ SuperClass, _ } ->
		    call_loop(Obj, Method, Args, 
			      SuperClass, [TryModule|TriedRev], FirstObj)
	    end
    end.
    
inspect(Obj) ->
    DeepList = inspect_loop(Obj, "#<"),
    lists:flatten(DeepList).

inspect_loop(Obj, Str) ->
    New = 
	atom_to_list(?CLASS(Obj)) ++
	": " ++
	(?CLASS(Obj)):inspect(Obj),
    
    case get_superobject(Obj) of
	{ ok, Superobj } ->
	    inspect_loop(Superobj, Str ++ New ++ " | ");
	none ->
	    Str ++ New ++ ">"
    end.
    
%% TODO: voids take only ok as return? 
start_new(Class, Args) ->
    case gen_server:start_link(thrift_oop_server, {Class, Args}, []) of
	{ok, Pid} ->
	    Pid
    end.
