%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

%%%
%%% dox:
%%%
%%% C++                   <-> Erlang
%%% classes                   modules
%%%   class b : public a        a:super() -> b.
%%%

-module(oop).

-export([start_new/2, get/2, set/3, call/2, call/3, inspect/1, class/1, is_object/1, is_a/2]).
-export([call1/3]). %% only for thrift_oop_server ... don't use it
-export([behaviour_info/1]).

-include("thrift.hrl").
-include("oop.hrl").

%% state for the call loop
-record(cstate, {
          obj,      %% the current object (on which we want to invoke MFA)
          module,   %% the current module we're considering
          func,     %% the method name (i.e. the function we're trying to invoke in Module)
          args,     %% the arguments, the first of which is Obj
          tried,    %% a (backwards) list of modules we've tried
          first_obj %% the original object
         }).

%%%
%%% behavior definition
%%%

behaviour_info(callbacks) ->
    [ {attr, 4},
      {super, 0}
     ];
behaviour_info(_) ->
    undefined.

%%%
%%% public interface
%%%

%% TODO: voids take only ok as return?
start_new(none=Resv, _) ->
    ?ERROR("can't instantiate ~p: class name is a reserved word", [Resv]),
    error;
start_new(Class, Args) ->
    {ok, Pid} = gen_server:start_link(thrift_oop_server, {Class, Args}, []),
    Pid.

%% get(Obj, Field) -> term()
%% looks up Field in Obj or its ancestor objects
get(Obj, Field) ->
    call(Obj, attr, [get, Field, get]).

set(Obj, Field, Value) -> %% TODO: could be tail-recursive
    Module = ?CLASS(Obj),
    case apply_if_defined(Module, attr, [Obj, set, Field, Value]) of
        {ok, V} -> V;
        undef   ->
            case get_superobject(Obj) of
                {ok, Superobj} ->
                    Superobj1 = set(Superobj, Field, Value),
                    Module:attr(Obj, set, super, Superobj1);
                undef ->
                    error(missing_attr_set, Field, Obj)
            end
    end.

%%
%% ** dynamic method dispatch **
%%
%% calls Module:Func(*Args) if it exists
%% if not, Module <- Module:super() and try again recursively
%%
%% Module:attr(*Args) is handled specially:
%% Obj needs to be replaced with Obj's "superobject"
%%
call(Obj, Func) ->
    call(Obj, Func, []).

call(Obj, Func, ArgsProper) ->
    %% this is WAY too expensive
    %% ?INFO("oop:call called: Obj=~p Func=~p ArgsProper=~p", [inspect(Obj), Func, ArgsProper]),
    case call1(Obj, Func, ArgsProper) of
        {ok, Value}       -> Value;
        {error, Kind, S1} -> error(Kind, S1)
    end.

call1(Obj, Func, ArgsProper) ->
    S = #cstate{
      obj       = Obj,
      module    = ?CLASS(Obj),
      func      = Func,
      args      = [Obj|ArgsProper], %% prepend This to args
      tried     = [],
      first_obj = Obj
     },
    call1(S).

call1(S = #cstate{obj=Obj, module=Module, func=Func, args=Args}) ->
    %% ?INFO("call1~n obj=~p~n MFA=~p, ~p, ~p", [inspect(Obj), Module, Func, Args]),
    %% io:format("call ~p~n", [Module]),
    case apply_if_defined(Module, Func, Args) of
        {ok, Value} -> {ok, Value};
        undef       -> call1_try_super(S)
    end.

call1_try_super(S = #cstate{func=attr, module=Module, tried=Tried}) ->
    case Module:super() of
        none       -> {error, missing_attr, S};
        Superclass -> call1_try_super_attr(Superclass, S)
    end;
call1_try_super(S = #cstate{func=Func, module=Module, tried=Tried}) ->
    case Module:super() of
        none       -> {error, missing_method, S};
        Superclass ->
            S1 = S#cstate{
                   module = Superclass,
                   tried  = [Module|Tried]
                  },
            call1(S1)
    end.

call1_try_super_attr(Superclass, S = #cstate{obj=Obj, module=Module, args=Args, tried=Tried}) ->
    %% look for attrs in the "super object"
    case get_superobject(Obj) of
        undef -> {error, missing_superobj, S};
        {ok, Superobj} when Module == ?CLASS(Obj) ->
            %% replace This with Superobj
            S1 = S#cstate{
                   obj    = Superobj,
                   args   = [Superobj|tl(Args)],
                   module = Superclass,
                   tried  = [Module|Tried]
                  },
            call1(S1)
    end.

%% careful: not robust against records beginning with a class name
%% (note: we can't just guard with is_record(?CLASS(Obj), Obj) since we
%% can't/really really shouldn't require all record definitions in this file
inspect(Obj) ->
    try
        case is_object(Obj) of
            true ->
                DeepList = inspect1(Obj, "#<"),
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

inspect1(Obj, Str) ->
    Class   = ?CLASS(Obj),
    Inspect = Class:inspect(Obj),
    Current = atom_to_list(Class) ++ ": " ++ Inspect,

    case get_superobject(Obj) of
        {ok, Superobj} ->
            inspect1(Superobj, Str ++ Current ++ " | ");
        undef ->
            Str ++ Current ++ ">"
    end.

%% class(Obj) -> atom() = Class
%%             | none
class(Obj) when is_tuple(Obj) ->
    %% if it's an object its first element will be a class name, and it'll have super/0
    case apply_if_defined(?CLASS(Obj), super, []) of
        {ok, _} -> ?CLASS(Obj);
        undef   -> none
    end;
class(_)    -> none.

%% is_a relationship
is_a(Obj, Class) ->
    %% ?INFO("is_a ~p ~p", [Obj, Class]),
    case is_object(Obj) of
        true ->
            is_a1(Obj, Class);
        false ->
            false
    end.
is_a1(Obj, Class) when Class == ?CLASS(Obj) ->
    true;
is_a1(Obj, Class) ->
    case get_superobject(Obj) of
        undef ->
            false;
        {ok, SuperObj} ->
            is_a1(SuperObj, Class)
    end.

%% is the tuple/record an object?
%% is_object(Obj) = bool()
is_object(Obj) when is_tuple(Obj) ->
    case class(Obj) of
        none -> false;
        _    -> true
    end;
is_object(_) -> false.

%%%
%%% private helpers
%%%

%% apply_if_defined(MFA) -> {ok, apply(MFA)}
%%                        | undef
%% this could be worth some money
apply_if_defined(M, F, A) ->
    apply_if_defined({M,F,A}).

apply_if_defined({M,F,A} = MFA) ->
    try
        %% io:format("apply ~p ~p ~p~n", [M,F,A]),
        {ok, apply(M, F, A)}
    catch
        _:Kind when Kind == undef; Kind == function_clause ->
            case erlang:get_stacktrace() of
                %% the first stack call should match MFA when `apply' fails because the function is undefined
                %% they won't match if the function is currently running and an error happens in the middle
                [MFA|_] -> undef;           % trapped successfully
                ST ->
                    io:format("DONIT THE EXIT THING ~p~n", [Kind]),
                    exit({Kind, ST}) % some unrelated error, re-exit
            end
    end.

get_superobject(Obj) ->
    apply_if_defined(?CLASS(Obj), attr, [Obj, get, super, get]).

%%%
%%% errors
%%%

tried(S = #cstate{module=Module, tried=Tried}) ->
    lists:reverse([Module|Tried]).

error(missing_superobj, S = #cstate{obj=Obj}) ->
    exit({missing_superobj, {inspect(Obj), tried(S)}});
error(missing_method, S = #cstate{obj=Obj, func=Func, args=Args}) ->
    exit({missing_method, {Func, inspect(Obj), tl(Args), tried(S)}});
error(missing_attr, S = #cstate{args=Args, first_obj=FirstObj}) ->
    exit({missing_attr, {hd(tl(Args)), inspect(FirstObj), tried(S)}}).

error(missing_attr_set, Field, Obj) ->
    BT = "..", %% TODO: give a backtrace
    exit({missing_attr, {Field, inspect(Obj), BT}}).
