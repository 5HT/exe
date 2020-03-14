% Copyright Groupoid Infinity, Inc.

Definitions.

A = [a-zA-Z_0-9\-\x{2074}-\x{208E}\x{2010}-\x{2191}\x{2193}-\x{2199}\x{2201}-\x{25FF}\x{3B1}-\x{3BA}\x{3BC}-\x{3FF}]
S = ([\t\s\r\n]|--.*)
B = [\r\n]
Star    = \*
Unit    = \(\)
Slash   = \\
Dot     = \.
Comma   = \,
Arrow   = (\-\>|\→)
Forall  = (\\/|\∀)
Meet    = (/\\)
Lambda  = (\\|\λ)
Curly   = [\{\}]
Angle   = [\<\>]
Parens  = [\(\)]
Square  = [\[\]]
Colon   = \:
Et      = \@
Eq      = \=
Pipe    = \|

Rules.

(data|record|\.1|\.2|split|comp|glue|fill|Glue|unglue) : {token,{list_to_atom(TokenChars),TokenLine}}.
(let|in|module|import|where) : {token,{list_to_atom(TokenChars),TokenLine}}.
(spawn|send|receive) : {token,{list_to_atom(TokenChars),TokenLine}}.
({Curly}|{Parens}|{Angle}|{Square}) : {token,{list_to_atom(TokenChars),TokenLine}}.
({Dot}|{Comma}|{Eq}|{Colon}|{Pipe}|{Star}|{Et}) : {token,{list_to_atom(TokenChars),TokenLine}}.

{A}+ : {token, {id, TokenLine, TokenChars}}.
{Arrow} : {token, {arrow, TokenLine}}.
{Forall} : {token, {forall, TokenLine}}.
{Meet} : {token, {meet, TokenLine}}.
{Lambda} : {token, {lam, TokenLine}}.
{B}+ : {token, {skip, TokenLine}}.
{S}+ : skip_token.

Erlang code.
