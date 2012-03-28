% file: "tak.erl"

-module(tak).
-export([start/0]).

tak(X,Y,Z) ->
  if
    Y<X -> tak( tak(X-1,Y,Z),
                tak(Y-1,Z,X),
                tak(Z-1,X,Y) );
    true -> Z
  end.

loop(0,R) -> R;
loop(N,R) -> loop(N-1,tak(18,12,6)).

start() ->
  statistics(runtime),
  R = loop(1000,0),
  {_,Time} = statistics(runtime),
  io:nl(),
  io:format("runtime = "),
  io:write(Time),
  io:format(" msecs"),
  io:nl(),
  io:format("result = "),
  io:write(R),
  io:nl().
