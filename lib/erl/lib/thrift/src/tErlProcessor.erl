%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tErlProcessor).

-include("thrift.hrl").
-include("oop.hrl").
-include("tErlProcessor.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/2, process/3]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super);
?DEFINE_ATTR(generatedProcessor);
?DEFINE_ATTR(handler).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tProcessor.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(generatedProcessor) ++ ", " ++
    ?FORMAT_ATTR(handler).

%%%
%%% class methods
%%%

new(GP, Handler) ->
    Super = (super()):new(),
    #?MODULE{super = Super, generatedProcessor = GP, handler = Handler}.

%% processor is generated code
%% handler is user code

%%%
%%% instance methods
%%%

process(This, Iprot, Oprot) ->
    GP      = oop:get(This, generatedProcessor),
    Handler = oop:get(This, handler),

    apply(GP, process, [Handler, Iprot, Oprot]).
