% file: "nrev.erl"

-module(nrev).
-export([start/0]).

nrev([H|T]) -> app(nrev(T),[H]);
nrev([])    -> [].

app([H|T],L) -> [H|app(T,L)];
app([],L)    -> L.

iota(N) -> iota(N,[]).
iota(0,L) -> L;
iota(N,L) -> iota(N-1,[N|L]).

loop(0,L,R) -> R;
loop(N,L,R) -> loop(N-1,L,nrev(L)).

start() ->
  L = iota(100),
  statistics(runtime),
  R = loop(20000,L,0),
  {_,Time} = statistics(runtime),
  io:nl(),
  io:format("runtime = "),
  io:write(Time),
  io:format(" msecs"),
  io:nl(),
  io:format("result = "),
  io:write(R),
  io:nl().
