-module(homotopy).
-description('Homotopy Type System').
-vsn("1.3.1").
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1]).
-compile(export_all).

main(A)     -> unicode(), case A of [] -> halt(help()); A -> halt(lists:sum(console(A))) end.
atom(X)     -> list_to_existing_atom(lists:concat([X])).
version()   -> proplists:get_value(vsn,module_info(attributes)).
unicode()   -> io:setopts(standard_io, [{encoding, unicode}]).
init([])    -> {ok, {{one_for_one, 5, 10}, []}}.
start()     -> start(normal,[]).
start(_,_)  -> unicode(), supervisor:start_link({local,cub},cub,[]).
stop(_)     -> ok.

console(S) ->
  lists:foldr(fun(I,_) ->
    R = lists:reverse(I),
    Res = lists:foldl(fun(X,A) -> console:(atom(X))(A) end,hd(R),tl(R)),
    io:format("~tp~n",[Res]),
    [] end, [], string:tokens(S,[","])).

help() ->
  io:format("HTS CTT-CCHM Homotopy Type System ~s~n~n",[version()]),
  io:format(" usage = homotopy args ~n"),
  io:format("  args = [] | cmd | cmd , args ~n"),
  io:format("   cmd = parse <tokens> | lex <string> | read <name> ~n"),
  io:format("       | fst <tuple> | snd <tuple> | a <name> | file <name> ~n"), 0.
