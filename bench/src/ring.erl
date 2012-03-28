% file: "ring.erl"

-module(ring).
-export([start/0,process/1]).

process(Main) ->
  receive
    Dest -> process(Main,Dest)
  end.

process(Main,Dest) ->
  receive
    terminate -> Dest ! terminate;
    0         -> Dest ! terminate,
                 receive
                   terminate -> Main ! done
                 end;
    X         -> Dest ! (X-1), process(Main,Dest)
  end.

create(Main,0) -> [];
create(Main,N) -> [spawn(ring,process,[Main])|create(Main,N-1)].

connect(Ps) -> connect(hd(Ps),Ps).
connect(First,[P])        -> P ! First;
connect(First,[P|Others]) -> P ! hd(Others), connect(First,Others).

ring(Nbprocs,Hops) ->
  Ps = create(self(),Nbprocs),
  connect(Ps),
  hd(Ps) ! Hops,
  receive
    done -> ok
  end.

loop(0,R) -> R;
loop(N,R) -> loop(N-1,ring(10,100000)).

start() ->
  statistics(runtime),
  R = loop(100,0),
  {_,Time} = statistics(runtime),
  io:nl(),
  io:format("runtime = "),
  io:write(Time),
  io:format(" msecs"),
  io:nl(),
  io:format("result = "),
  io:write(R),
  io:nl().
