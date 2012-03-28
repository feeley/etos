% file: "huff.erl"

-ifdef(ETOS).
-define(IS_LIST(X),is_list(X)).
-define(CHAR_TO_INTEGER(X),char_to_integer(X)).
-else.
-define(IS_LIST(X),list(X)).
-define(CHAR_TO_INTEGER(X),X).
-endif.

-module(huff).
-export([start/0]).

pack_unpack(Data) ->
  OrgSize = length(Data),
  FT = build_freq_trees(Data),
  Trees = sort_trees(FT, []),
  CodeTree = build_code_tree(Trees),
  Codes = flatten(make_codes(CodeTree, [])),
  Bits = flatten(pack_data(Codes, Data)),
  Bytes = bits_to_bytes(Bits),
  unpack(OrgSize, CodeTree, bytes_to_bits(Bytes)).

unpack(0, CodeTree, Bits) ->
  [];
unpack(Size, CodeTree, Bits) ->
  {Byte, RestBits} = find_byte(CodeTree, Bits),
  [Byte | unpack(Size-1, CodeTree, RestBits)].

find_byte({_, leaf, Byte}, Bits) ->
  {Byte, Bits};
find_byte({_, L, R}, [1|Bits]) ->
  find_byte(L, Bits);
find_byte({_, L, R}, [0|Bits]) ->
  find_byte(R, Bits).

bytes_to_bits([]) ->
  [];
bytes_to_bits([Byte|Bytes]) ->
  B7 = Byte div 128,
  B6 = (Byte div 64) rem 2,
  B5 = (Byte div 32) rem 2,
  B4 = (Byte div 16) rem 2,
  B3 = (Byte div 8) rem 2,
  B2 = (Byte div 4) rem 2,
  B1 = (Byte div 2) rem 2,
  B0 = Byte rem 2,
  [B7,B6,B5,B4,B3,B2,B1,B0 | bytes_to_bits(Bytes)].

bits_to_bytes([B7, B6, B5, B4, B3, B2, B1, B0 | Rest]) ->
  [(B7*128+B6*64+B5*32+B4*16+B3*8+B2*4+B1*2+B0) | bits_to_bytes(Rest)];
bits_to_bytes([B7, B6, B5, B4, B3, B2, B1]) ->
  [B7*128+B6*64+B5*32+B4*16+B3*8+B2*4+B1*2];
bits_to_bytes([B7, B6, B5, B4, B3, B2]) ->
  [B7*128+B6*64+B5*32+B4*16+B3*8+B2*4];
bits_to_bytes([B7, B6, B5, B4, B3]) ->
  [B7*128+B6*64+B5*32+B4*16+B3*8];
bits_to_bytes([B7, B6, B5, B4]) ->
  [B7*128+B6*64+B5*32+B4*16];
bits_to_bytes([B7, B6, B5]) ->
  [B7*128+B6*64+B5*32];
bits_to_bytes([B7, B6]) ->
  [B7*128+B6*64];
bits_to_bytes([B7]) ->
  [B7*128];
bits_to_bytes([]) ->
  [].

pack_data(Codes, []) ->
  [];
pack_data(Codes, [Byte|Rest]) ->
  [get_code(Byte, Codes) | pack_data(Codes, Rest)].

get_code(Index, []) ->
  error;
get_code(Index, [{I, Bits}|_]) when Index == I ->
  Bits;
get_code(Index, [_|Rest]) ->
  get_code(Index, Rest).

make_code_index([], Done) ->
  Done;
make_code_index([C|Rest], Done) ->
  make_code_index(Rest, insert_code(C, Done)).

insert_code(C, []) ->
  [C];
insert_code({V1, B1}, [{V2, B2}|Rest]) when V1 < V2 ->
  [{V1, B1}, {V2, B2}|Rest];
insert_code({V1, B1}, [{V2, B2}|Rest]) ->
  [{V2, B2} | insert_code({V1, B1}, Rest)].

make_codes({_, leaf, Byte}, Bits) ->
  {Byte, reverse(Bits)};
make_codes({_, R, L}, Bits) ->
  [make_codes(R, [1|Bits]), make_codes(L, [0|Bits])].

build_code_tree([Tree]) ->
  Tree;
build_code_tree([{Val1, R1, L1}, {Val2, R2, L2} | Rest]) ->
  build_code_tree(insert_tree({Val1+Val2, {Val1, R1, L1}, {Val2, R2, L2}},
                              Rest)).

sort_trees([], Sorted) ->
  Sorted;
sort_trees([T|Trees], Sorted) ->
  sort_trees(Trees, insert_tree(T, Sorted)).

insert_tree({0, _, _}, []) ->
  [];
insert_tree(Tree, []) ->
  [Tree];
insert_tree({0, _, _}, Trees)  ->
  Trees;
insert_tree({Val1, R1, L1}, [{Val2, R2, L2}|Rest]) when Val1 < Val2 ->
  [{Val1, R1, L1}, {Val2, R2, L2}|Rest];
insert_tree(T1, [T2|Rest]) ->
  [T2|insert_tree(T1, Rest)].

build_freq_trees(Data) ->
  build_freq_table(Data, 0).

build_freq_table(_, 256) ->
  [];
build_freq_table(Data, X) ->
  [{occurs(X, Data, 0), leaf, X} | build_freq_table(Data, X+1)].

occurs(X, [], Ack) ->
  Ack;
occurs(X, [Y|Rest], Ack) when X == Y ->
  occurs(X, Rest, Ack+1);
occurs(X, [_|Rest],Ack) ->
  occurs(X, Rest, Ack).

reverse(X) ->
  reverse(X, []).
reverse([H|T], Y) ->
  reverse(T, [H|Y]);
reverse([], X) ->
  X.

append([H|T], Z) ->
  [H|append(T, Z)];
append([], X) ->
  X.

flatten(List) ->
  flatten(List, []).

flatten([H|T], Cont) when ?IS_LIST(H) ->
  flatten(H, [T|Cont]);
flatten([H|T], Cont) ->
  [H|flatten(T, Cont)];
flatten([], [H|Cont]) ->
  flatten(H, Cont);
flatten([], []) ->
  [].

loop(0,Data,R) -> R;
loop(N,Data,R) -> loop(N-1,Data,pack_unpack(Data)).

str_to_intlist([]) ->
    [];
str_to_intlist([H|T]) -> 
    [?CHAR_TO_INTEGER(H)|str_to_intlist(T)].

start() ->
  Data = str_to_intlist("Beauty is in the eyes of the beholder."),
  statistics(runtime),
  R = loop(5000,Data,0),
  {_,Time} = statistics(runtime),
  io:nl(),
  io:format("runtime = "),
  io:write(Time),
  io:format(" msecs"),
  io:nl(),
  io:format("result = "),
  io:write(R),
  io:nl().
