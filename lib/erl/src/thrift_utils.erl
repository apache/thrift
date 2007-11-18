%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift_utils).

-include("transport/tTransportException.hrl").

-export([tabulate/2, dict_size/1, sformat/2, unbrack/1, first_item/1, unnest_record/2]).

%% tabulate
tabulate(N,F) ->
    tabulate(0, N, F).

tabulate(N,M,_) when N==M ->
    [];
tabulate(N,M,F) ->
    [F(N) | tabulate(N+1, M, F)].

%% makin me sad
dict_size(Dict) ->
  dict:fold(fun (_,_,I) -> I+1 end,0,Dict).

%% I CAN HAS EAZIER KTHX
sformat(Format, Data) when is_list(Data) ->
    lists:flatten(io_lib:format(Format, Data));
sformat(Format, Item) ->
    error_logger:warning_msg("sformat called with non-list Data: (~p, ~p)", [Format, Item]),
    sformat(Format, [Item]).

%% render a list and pick off the square brackets
unbrack(List) ->
    List1 = sformat("~w", [List]),
    string:substr(List1, 2, length(List1)-2).

first_item(DeepTuple) when is_tuple(DeepTuple) ->
    first_item(element(1, DeepTuple));
first_item(NotTuple) ->
    NotTuple.

unnest_record(Term, RecordTag) ->
    case is_record(Term, RecordTag) of
        true ->
            {ok, Term};
        false when is_tuple(Term) ->
            unnest_record(element(1, Term), RecordTag);
        _ ->
            error
    end.
