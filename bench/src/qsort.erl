% file: "qsort.erl"

-module(qsort).
-export([start/0]).

qsort(L) -> qsort(L,[]).
qsort([H|T],L) -> partition(H,T,[],[],L);
qsort([],L)    -> L.

partition(Pivot,[H|T],A,B,L) when H<Pivot ->
  partition(Pivot,T,[H|A],B,L);
partition(Pivot,[H|T],A,B,L) ->
  partition(Pivot,T,A,[H|B],L);
partition(Pivot,[],A,B,L) ->
  qsort(A,[Pivot|qsort(B,L)]).

loop(0,L,R) -> R;
loop(N,L,R) -> loop(N-1,L,qsort(L)).

start() ->
  L = [27,74,17,33,94,18,46,83,65, 2,
       32,53,28,85,99,47,28,82, 6,11,
       55,29,39,81,90,37,10, 0,66,51,
        7,21,85,27,31,63,75, 4,95,99,
       11,28,61,74,18,92,40,53,59, 8],
  statistics(runtime),
  R = loop(50000,qsort(L),0),
  {_,Time} = statistics(runtime),
  io:nl(),
  io:format("runtime = "),
  io:write(Time),
  io:format(" msecs"),
  io:nl(),
  io:format("result = "),
  io:write(R),
  io:nl().
