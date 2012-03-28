% file: "length.erl"

-module(length).
-export([start/0]).

len(L) -> len(0,L).
len(X,[H|T]) -> len(X+1,T);
len(X,[]) -> X.

make_list(X) -> make_list(X,[]).
make_list(0,L) -> L;
make_list(X,L) -> make_list(X-1,[0|L]).

loop(0,L,R) -> R;
loop(N,L,R) -> loop(N-1,L,len(L)).

start() ->
  L = make_list(2000),
  statistics(runtime),
  R = loop(100000,L,0),
  {_,Time} = statistics(runtime),
  io:nl(),
  io:format("runtime = "),
  io:write(Time),
  io:format(" msecs"),
  io:nl(),
  io:format("result = "),
  io:write(R),
  io:nl().
