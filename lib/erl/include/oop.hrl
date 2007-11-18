%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-define(CLASS(Obj), element(1, Obj)).

-define(DEFINE_ATTR(Attr), attr(This, get, Attr, _Value) -> This#?MODULE.Attr;
                           attr(This, set, Attr, Value)  -> This#?MODULE{Attr=Value}
).

%%% static: use only if you're sure This is class ?MODULE and not a super/subclass
-define(ATTR(Attr), This#?MODULE.Attr).

%%% convenience for implementing inspect/1
%%% e.g. -> "foo=5"
-define(FORMAT_ATTR(Attr),
        io_lib:write_atom(Attr) ++ "=" ++ io_lib:print(?ATTR(Attr))
).

-define(ATTR_DUMMY,
        attr(dummy, dummy, dummy, dummy) ->
               exit(dummy_attr_used)
).
