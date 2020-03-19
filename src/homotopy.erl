-module(homotopy).
-description('Homotopy Type System').
-vsn("1.3.1").
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1]).
-compile(export_all).

unicode()    -> io:setopts(standard_io, [{encoding, unicode}]).
main(A)      -> unicode(), case A of [] -> halt(help()); A -> halt(lists:sum(console(A))) end.
start()      -> start(normal,[]).
start(_,_)   -> unicode(), supervisor:start_link({local,cub},cub,[]).
stop(_)      -> ok.
init([])     -> {ok, {{one_for_one, 5, 10}, []}}.
return(true) -> 1;
return(false)-> 0.
version()    -> proplists:get_value(vsn,module_info(attributes)).

help() ->
  io:format("HTS CTT-CCHM Homotopy Type System ~s~n",[version()]),
  io:format("~n"),
  io:format(" usage = homotopy args ~n"),
  io:format("  args = [] | cmd | cmd , args ~n"),
  io:format("   cmd = parse <tokens> | file <name> | fst <tuple> | snd <tuple> ~n"),
  return(false).

console(S) ->
  lists:foldr(fun(I,_) ->
    R = rev(I),
    Res = lists:foldl(fun(X,A) -> ?MODULE:(atom(X))(A) end,hd(R),tl(R)),
    io:format("~tp~n",[Res]),
    [] end, [], string:tokens(S,[","])).

rev(X)           -> lists:reverse(X).
flat(X)          -> lists:flatten(X).
tokens(X,Y)      -> string:tokens(X,Y).
atom(X)          -> list_to_atom(cat([X])).
cat(X)           -> lists:concat(X).
fst({A,_})       -> A.
snd({_,B})       -> B.
keyget(K,D)      -> proplists:get_value(K,D).
keyget(K,D,I)    -> lists:nth(I+1,proplists:get_all_values(K,D)).

convert(A,S, nt) -> convert(A,S);
convert(A,_, _)  -> A.

convert([],Acc) -> rev(Acc);
convert([$>|T],Acc) -> convert(T,[61502|Acc]);
convert([$<|T],Acc) -> convert(T,[61500|Acc]);
convert([$:|T],Acc) -> convert(T,[61498|Acc]);
convert([$||T],Acc) -> convert(T,[61564|Acc]);
convert([H|T],Acc)  -> convert(T,[H|Acc]).

a(X)       -> macro:a(X).
parse(X)   -> io:format("~tp~n",[macro:parse(bin(X))]).
bin(X)     -> unicode:characters_to_list(X).

file(F) -> case file:read_file(convert(F,[],element(2,os:type()))) of
                {ok,Bin} -> bin(Bin);
                {error,_} -> mad(F) end.

mad(F)  -> case mad_repl:load_file(F) of
                {ok,Bin} -> Bin;
                {error,_} -> erlang:error({"File not found",F}) % <<>>
            end.

pad(D) -> lists:duplicate(D*7," ").
