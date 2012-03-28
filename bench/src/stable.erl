% file: "stable.erl"

-module(stable).
-export([start/0,man/2,woman/2]).

man(God,Id) ->
  receive
    Women -> cruise(God,Id,order(Id,Women))
  end.

cruise(God,Id,[Woman|Rest]) ->
  Woman ! {proposal,self()},
  receive
    reject ->
      cruise(God,Id,Rest);
    {mariage,Woman_id} ->
      God ! {self(),Woman_id}
  end.

woman(God,Id) ->
  receive
    Men ->
      receive
        {proposal,Mate} ->
          God ! {engaged,self()},
          improve_mate(Id,Mate,order(Id,Men))
      end
  end.

improve_mate(Id,Mate,Prefs) ->
  receive
    stable ->
      Mate ! {mariage,Id};
    {proposal,Offer} ->
      case preferable(Offer,Mate,Prefs) of
        true -> Mate ! reject,
                improve_mate(Id,Offer,Prefs);
        false -> Offer ! reject,
                 improve_mate(Id,Mate,Prefs)
      end
  end.

preferable(Man1,Man2,[Man1|_]) -> true;
preferable(Man1,Man2,[Man2|_]) -> false;
preferable(Man1,Man2,[_|Rest]) -> preferable(Man1,Man2,Rest).

order(Id,L) -> order(next_random(Id),L,length(L)).

order(Random,L,0) -> [];
order(Random,L,N) ->
  X = (Random rem N) + 1,
  [nth(X,L)|order(next_random(Random),remove_nth(X,L),N-1)].

next_random(Random) -> (Random * 1713 + 9363) rem 10067.

nth(1,[H|T]) -> H;
nth(N,[H|T]) -> nth(N-1,T).

remove_nth(1,[H|T]) -> T;
remove_nth(N,[H|T]) -> [H|remove_nth(N-1,T)].

wait_until_stable([]) ->
  ok;
wait_until_stable([Woman|Rest]) ->
  receive
    {engaged,Woman} -> wait_until_stable(Rest)
  end.

mariages([]) ->
  [];
mariages([Man|Rest]) ->
  receive
    {Man,Woman_id} -> [Woman_id|mariages(Rest)]
  end.

broadcast([],Msg) ->
  ok;
broadcast([H|T],Msg) ->
  H ! Msg,
  broadcast(T,Msg).

create_men(God,0) -> [];
create_men(God,N) -> [spawn(stable,man,[God,N])|create_men(God,N-1)].

create_women(God,0) -> [];
create_women(God,N) -> [spawn(stable,woman,[God,N])|create_women(God,N-1)].

stable(N) ->
  God = self(),
  Men = create_men(God,N),
  Women = create_women(God,N),
  broadcast(Women,Men),
  broadcast(Men,Women),
  wait_until_stable(Women),
  broadcast(Women,stable),
  mariages(Men).

loop(0,R) -> R;
loop(N,R) -> loop(N-1,stable(10)).

start() ->
  statistics(runtime),
  R = loop(5000,0),
  {_,Time} = statistics(runtime),
  io:nl(),
  io:format("runtime = "),
  io:write(Time),
  io:format(" msecs"),
  io:nl(),
  io:format("result = "),
  io:write(R),
  io:nl().
