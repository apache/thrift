%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tException).

-include("oop.hrl").
-include("thrift.hrl").
-include("tException.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/2, add_backtrace_element/2, throw/2, inspect_with_backtrace/2, inspect_with_backtrace/3]).

-export([read/1]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(message);
?DEFINE_ATTR(type);
?DEFINE_ATTR(backtrace).

%%%
%%% behavior callbacks
%%%

%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    none.

%%% inspect(This) -> string()

inspect(This) ->
    BT = ?ATTR(backtrace),
    Depth =
        if
            is_list(BT) -> integer_to_list(length(BT));
            true -> "?"
        end,
    ?FORMAT_ATTR(message) ++ ", " ++
    ?FORMAT_ATTR(type)    ++ ", "
        " backtrace:" ++ Depth.

%%%
%%% class methods
%%%

new(Type, Message) ->
    #?MODULE{type=Type, message=Message, backtrace=[]}.

add_backtrace_element(E, Info) ->
    BT = oop:get(E, backtrace),
    E1 = oop:set(E, backtrace, [Info|BT]),
    E1.

throw(Class, Args) ->
    E = apply(Class, new, Args),
    exit({thrift_exception, E}).


inspect_with_backtrace(E, Where, Info) ->
    E1 = add_backtrace_element(E, Info),
    inspect_with_backtrace(E1, Where).

inspect_with_backtrace(E, Where) ->
    thrift_utils:sformat("** ~s~n** ~s", [Where, oop:inspect(E)]) ++
        case oop:get(E, backtrace) of
            [] ->
                "";
            BT when is_list(BT) ->
                thrift_utils:sformat("~n** trace = ~p", [lists:reverse(BT)]);
            Else ->
                thrift_utils:sformat("<ERROR BT NOT A LIST = ~p>", [Else])
        end.

read(E) ->
    case oop:class(E) of
        none ->
            none;
        Class ->
            Type = oop:get(E, type),
            BT   = oop:get(E, backtrace),
            {Class, Type, BT}
    end.
