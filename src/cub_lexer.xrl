% Copyright Groupoid Infinity, Inc.

Definitions.

A = [\'a-zA-Z_0-9\x{2074}-\x{208E}\x{2010}-\x{2191}\x{2193}-\x{2199}\x{2201}-\x{25FF}\x{3B1}-\x{3BA}\x{3BC}-\x{3FF}]
S = ([\t\s\r\n]|--.*)
B = [\r\n]

Join    = (\x{5C}\x{2F})
Meet    = (\x{2F}\x{5C})
Unit    = (\(\))
Arrow   = (\-\>)
Curly   = \{|\}
Angle   = \<|\>
Parens  = \(|\)
Square  = \[|\]
Lambda  = \\|\λ
Minus   = \-
Comma   = \,
Colon   = \:
Et      = \@
Eq      = \=
Dot     = \.
Pipe    = \|
Star    = \*
Slash   = \\

Rules.

(data|\.1|\.2|split|comp|glue|fill|Glue|unglue|let|in|module|import|where|spawn|send|receive)
  : {token,{list_to_atom(TokenChars),TokenLine}}.

({Curly}|{Parens}|{Angle}|{Square}|{Dot}|{Comma}|{Eq}|{Colon}|{Pipe}|{Star}|{Et}|{Minus})
  : {token,{list_to_atom(TokenChars),TokenLine}}.

{Arrow}  : {token, {arrow, TokenLine}}.
{Join}   : {token, {join, TokenLine}}.
{Meet}   : {token, {meet, TokenLine}}.
{Lambda} : {token, {lam, TokenLine}}.
{A}+     : {token, {id, TokenLine, TokenChars}}.
{B}+     : {token, {skip, TokenLine}}.
{S}+     : skip_token.

Erlang code.
