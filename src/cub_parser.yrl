% Copyright Groupoid Infinity, Inc.

mod -> 'module' id 'where' imp dec : {module,'$2','$4','$5'}.
imp -> skip imp : '$2'.
imp -> '$empty' : [].
imp -> 'import' id imp : [{import,'$2'}|'$3'].
ids -> '$empty' : [].
ids -> id ids : ['$1'|'$2'].
tele -> '$empty' : [].
tele -> cotele : '$1'.
br -> ids arrow exp : {br,'$1','$3'}.
br -> ids '@' ids arrow exp : {br,'$1','$3','$5'}.
brs -> '$empty' : [].
brs -> cobrs : '$1'.
cobrs -> '|' br brs : ['$2'|'$3'].
cotele -> '(' exp ':' exp ')' tele : {tele,uncons('$2'),'$4','$6'}.
sys -> '[' sides ']' : {sys,'$2'}.
sides -> '$empty' : [].
sides -> side : '$1'.
sides -> side ',' sides : ['$1'|'$3'].
side -> '(' id '=' id ')' arrow exp : {side,'$2','$4','$7'}.
formula -> formula forall f1 : {join,'$1','$3'}.
formula -> f1 : '$1'.
formula -> f2 : '$1'.
f1 -> f1 meet f2 : {meet,'$1','$3'}.
f1 -> f2 : '$1'.
f2 -> '-' f2 : {neg,'$2'}.
f2 -> id : '$1'.
exp -> 'split' cobrs : {split,'$2'}.
exp -> id : '$1'.
exp -> id '{' exp '}': {inst,'$1','$h3'}.
exp -> '<' ids '>' exp : {plam,uncons('$2'),'$4'}.
exp -> exp '.1' : {fst,'$1'}.
exp -> exp '.2' : {snd,'$1'}.
exp -> lam cotele arrow exp : {lam,'$2','$4'}.
exp -> cotele arrow exp : {pi,'$1','$3'}.
exp -> exp arrow exp : {pi,'$1','$3'}.
exp -> cotele '*' exp : {sigma,'$1','$3'}.
exp -> 'comp' exp exp sys : {comp,'$2','$3','$4'}.
exp -> 'fill' exp exp sys : {fill,'$2','$3','$4'}.
exp -> 'glue' exp sys : {glue,'$2','$3'}.
exp -> 'unglue' exp sys : {unglue,'$2','$3'}.
exp -> exp ',' exp : uncons_p('$1','$3').
exp -> papp : '$1'.
exp -> '(' exp ')' : '$2'.
exp -> app : '$1'.
app -> exp exp : {app,'$1','$2'}.
papp -> exp '@' formula : {papp,'$1','$3'}.
dec -> '$empty' : [].
dec -> codec : '$1'.
codec -> def skip dec : ['$1'|'$3'].
codec -> def dec : ['$1'|'$2'].
def -> 'data' id tele '=' sum : {data,'$2','$3','$5'}.
def -> id tele ':' exp '=' exp : {def,'$1','$2','$4','$6'}.
def -> id tele ':' exp '=' exp 'where' def : {def,'$1','$2','$4','$6','$8'}.
sum -> '$empty' : [].
sum -> rsum : '$1'.
rsum -> id tele : {ctor,'$1','$2'}.
rsum -> id tele '|' rsum : [{ctor,'$1','$2'}|'$4'].
rsum -> id tele '<' ids '>' sys : {htor,'$1','$2','$4','$6'}.
rsum -> id tele '<' ids '>' sys '|' rsum : [{htor,'$1','$2','$4','$6'}|'$8'].
Rootsymbol mod.
Right 100 exp.
Right 100 arrow.
Right 100 formula.
Left 10 '|'.
Left 10 app.
Left 10 skip.
Left 10 'data'.
Left 10 id.
Left 10 def.

Nonterminals mod imp tele exp app dec def ids sum cotele rsum
             br brs cobrs codec formula f1 f2 side sides sys papp.

Terminals id digits atom oper skip lam meet arrow forall
          '(' ')' '[' ']' '<' '>' '{' '}' '.' ','
          ':' '=' '#' '|' '-' '*' '/' '@' '0' '1'
          'module' 'where' 'import' 'record' 'data' 'split'
          'let' 'in' '.1' '.2'  'comp' 'fill' 'glue' 'unglue'.

Erlang code.

uncons_p({pair,L,R},C) -> {pair,L,uncons_p(R,C)};
uncons_p(A,B) -> {pair,A,B}.

uncons({app,L,R}) -> [R|uncons(L)];
uncons(X) -> [X].
