-module(tException).
-include("tException.hrl").
-export([new/0]).

new() ->
    #tException{}.

