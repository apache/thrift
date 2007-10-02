%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(oop).

-export([get/2, set/3, call/2, call/3, inspect/1, start_new/2, is_object/1, class/1]).
-export([behaviour_info/1]).

-include("thrift.hrl").
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

is_object(Obj) when is_tuple(Obj) ->
    try
	(?CLASS(Obj)):super(), %% if it's an object its first element will be a class name, and it'll have super/0
	true
    catch
	error:Kind when Kind == undef; Kind == function_clause ->
	    false
    end;
is_object(_) ->
    false.

call(Obj, Method, ArgsProper) ->
    %% error_logger:info_msg("call called: Obj=~p Method=~p ArgsProper=~p", [inspect(Obj), Method, ArgsProper]),
    Args = [Obj|ArgsProper], %% prepend This to args
    TryModule = ?CLASS(Obj),
    call_loop(Obj, Method, Args, TryModule, [], Obj).

call(Obj, Method) -> 
    call(Obj, Method, []).

call_loop(Obj, Method, Args, TryModule, TriedRev, FirstObj) ->
    try
	%% error_logger:info_msg("call_loop~n ~p~n ~p~n ~p~n ~p", [inspect(Obj), Method, Args, TryModule]),
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
    
class(Obj) when is_tuple(Obj) ->
    case is_object(Obj) of
	true ->
	    ?CLASS(Obj);
	false ->
	    none
    end;
class(_) ->
    none.

%% careful: not robust against records beginning with a class name
%% (note: we can't just guard with is_record(?CLASS(Obj), Obj) since we
%% can't/really really shouldn't require all record definitions in this file
inspect(Obj) ->
    try
	case is_object(Obj) of
	    true ->
		DeepList = inspect_loop(Obj, "#<"),
		lists:flatten(DeepList);
	    false ->
		thrift_utils:sformat("~p", [Obj])
	end
    catch
	_:E ->
	    thrift_utils:sformat("INSPECT_ERROR(~p) ~p", [E, Obj])

	    %% TODO(cpiro): bring this back once we're done testing:
	    %% _:E -> thrift_utils:sformat("~p", [Obj])
    end.

inspect_loop(Obj, Str) ->
    Class   = ?CLASS(Obj),
    Inspect = Class:inspect(Obj),
    Current = atom_to_list(Class) ++ ": " ++ Inspect,

    case get_superobject(Obj) of
	{ ok, Superobj } ->
	    inspect_loop(Superobj, Str ++ Current ++ " | ");
	none ->
	    Str ++ Current ++ ">"
    end.

%% TODO: voids take only ok as return? 
start_new(none=Resv, _) ->
    error_logger:format("can't instantiate ~p: class name is a reserved word", [Resv]),
    error;
start_new(Class, Args) ->
    {ok, Pid} = gen_server:start_link(thrift_oop_server, {Class, Args}, []),
    Pid.
