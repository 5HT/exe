-module(homotopy).
-description('Homotopy Type System').
-vsn("1.3.1").
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1, console/1]).

init([])   -> {ok, {{one_for_one, 5, 10}, []}}.
main(A)    -> console:unicode(), case A of [] -> halt(help()); A -> halt(console(A)) end.
start(_,_) -> console:unicode(), supervisor:start_link({local,?MODULE},?MODULE,[]).
stop(_)    -> ok.

errcode({error,_}) -> 1;
errcode(_)         -> 0.

console(S) ->
  lists:foldr(fun(I,Errors) -> R = lists:reverse(I),
    Res = lists:foldl(fun(X,A) -> console:(list_to_atom(lists:concat([X])))(A) end,hd(R),tl(R)),
    io:format("~tp~n",[Res]),
    Errors + errcode(Res) end, 0, string:tokens(S,[","])).

help() ->
  io:format("CTT-CCHM Homotopy Type System ~s~n~n",[proplists:get_value(vsn,module_info(attributes))]),
  io:format(" usage = homotopy args ~n"),
  io:format("  args = [] | cmd | cmd args ~n"),
  io:format("   cmd = parse <tokens> | lex <string> | read <name> ~n"),
  io:format("       | fst <tuple> | snd <tuple> | a <name> | file <name> ~n"), 0.
