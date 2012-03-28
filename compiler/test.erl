-module(test).
-export([start/0]).

f(X,Width,Height) -> X*X.

start() -> io:write([hello,f(10,20,30)]), io:nl().
