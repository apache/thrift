-module(thrift_utils).

-export([tabulate/2,dict_size/1]).

tabulateh(N,M,_) when N==M -> [];
tabulateh(N,M,F) -> [F(N)|tabulateh(N+1,M,F)].
tabulate(N,F) -> tabulateh(0,N,F).

% makin me sad
dict_size(Dict) ->
  dict:fold(fun (_,_,I) -> I+1 end,0,Dict).
