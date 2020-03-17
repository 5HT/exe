-module(macro).
-compile(export_all).

file(F) ->
  case file:read_file(F) of
    {ok,B} -> {ok,unicode:characters_to_list(B)};
    {error,E} -> {error,{file,F,E}} end.

lexer({error,S}) -> {error,S};
lexer({ok,S}) ->
  case cub_lexer:string(S) of
    {ok,T,_} -> {ok,T};
    {error,{L,A,X},_} -> {error,{lexer,L,A,element(2,X)}} end.

parser({error,T}) -> {error,T};
parser({ok,T}) ->
  case cub_parser:parse(T) of
    {ok,AST}        -> {ok,AST};
    {error,{L,A,S}} -> {error,{parser,L,A,S}} end.

parse(F) -> snd(parser(lexer({ok,F}))).
a(F) -> snd(parser(lexer(file(F)))).

fst({X,_}) -> X.
snd({error,X}) -> {error,X};
snd({_,[X]}) -> X;
snd({_,X}) -> X.
