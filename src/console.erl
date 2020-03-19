-module(console).
-description('Console Commands').
-compile(export_all).

fst({X,_}) -> X.
snd({_,X}) -> X.
file(F)    -> lex(read(F)).
a(F)       -> snd(parse(lex(read(F)))).

read(F) ->
  case file:read_file(F) of
    {ok,B} -> {ok,unicode:characters_to_list(B)};
    {error,E} -> {error,{file,F,E}} end.

lex({error,S}) -> {error,S};
lex({ok,S}) ->
  case cub_lexer:string(S) of
    {ok,T,_} -> {ok,T};
    {error,{L,A,X},_} -> {error,{lexer,L,A,element(2,X)}} end.

parse({error,T}) -> {error,T};
parse({ok,F}) ->
  case cub_parser:parse(F) of
    {ok,AST}        -> {ok,AST};
    {error,{L,A,S}} -> {error,{parser,L,A,S}} end.
