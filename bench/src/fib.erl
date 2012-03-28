% file: "fib.erl"

-module(fib).
-export([start/0]).

fib(0) -> 0;
fib(1) -> 1;
fib(X) -> fib(X-1) + fib(X-2).

loop(0,R) -> R;
loop(N,R) -> loop(N-1,fib(30)).

start() ->
  statistics(runtime),
  R = loop(50,0),
  {_,Time} = statistics(runtime),
  io:nl(),
  io:format("runtime = "),
  io:write(Time),
  io:format(" msecs"),
  io:nl(),
  io:format("result = "),
  io:write(R),
  io:nl().
