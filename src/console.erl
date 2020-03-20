-module(console).
-description('Console Commands').
-export([fst/1, snd/1, read/1, lex/1, parse/1, file/1, a/1, unicode/0, errcode/1]).

unicode()          -> io:setopts(standard_io, [{encoding, unicode}]).
errcode({ok,_})    -> 0;
errcode({error,_}) -> 1.

fst({X,_}) -> {ok,X}.
snd({_,X}) -> {ok,X}.
file(F)    -> lex(read(F)).
a(F)       -> parse(file(F)).

read(F) ->
  case file:read_file(F) of
    {ok,B} -> {ok,unicode:characters_to_list(B)};
    {error,E} -> {error,{file,F,E}} end.

lex({error,S}) -> {error,S};
lex({ok,S}) ->
  case lexer:string(S) of
    {ok,T,_} -> {ok,T};
    {error,{L,A,X},_} -> {error,{lexer,L,A,element(2,X)}} end.

parse({error,T}) -> {error,T};
parse({ok,F}) ->
  case parser:parse(F) of
    {ok,AST}        -> {ok,AST};
    {error,{L,A,S}} -> {error,{parser,L,A,S}} end.
