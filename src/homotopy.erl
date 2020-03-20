-module(homotopy).
-description('Homotopy Type System').
-vsn("1.3.1").
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1, console/1]).

init([])   -> {ok, {{one_for_one, 5, 10}, []}}.
main(A)    -> console:unicode(), halt(console(A)).
start(_,_) -> console:unicode(), supervisor:start_link({local,?MODULE},?MODULE,[]).
stop(_)    -> ok.

console([]) ->
  io:format("CTT-CCHM Homotopy Type System ~s~n~n",[proplists:get_value(vsn,module_info(attributes))]),
  io:format("   Usage := homotopy Args <filename> ~n"),
  io:format("    Args := Command | Command Args ~n"),
  io:format(" Command := parse | lex | read | fst | snd | file ~n~n"), 0;

console(S) ->
  lists:foldr(fun(I,Errors) -> R = lists:reverse(I),
    Res = lists:foldl(fun(X,A) -> console:(list_to_atom(lists:concat([X])))(A) end,hd(R),tl(R)),
    io:format("~tp~n",[element(2,Res)]), Errors + console:errcode(Res) end, 0, string:tokens(S,[","])).
