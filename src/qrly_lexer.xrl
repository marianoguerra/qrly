Definitions.

% numbers
Number      = [0-9]

% delimiters and operators
Open        = \(
Close       = \)
OpenList    = \[
CloseList   = \]
Sep         = ,
Whites      = \s+
Ops         = (\|=|~=|\*=|!=|\$=|\^=|=)
Child       = >
All         = \*
Sibling     = ~
Adjacent    = \+

% identifiers and strings
Identifier  = [a-zA-Z][a-zA-Z0-9\_\-]*

String      = "(\\\^.|\\.|[^\"])*"

Rules.

% numbers
{Number}+                : make_token(integer, TokenLine, TokenChars, fun erlang:list_to_integer/1).

% delimiters and operators
{Open}                   : make_token(open,        TokenLine, TokenChars).
{Close}                  : make_token(close,       TokenLine, TokenChars).
{OpenList}               : make_token(open_list,   TokenLine, TokenChars).
{CloseList}              : make_token(close_list,  TokenLine, TokenChars).

{Sep}                    : make_token(sep,          TokenLine, TokenChars).
{All}                    : make_token(all,          TokenLine, TokenChars).
{Ops}                    : make_token(op,           TokenLine, TokenChars).
{Child}                  : make_token(child,        TokenLine, TokenChars).
{Sibling}                : make_token(sibling,      TokenLine, TokenChars).
{Adjacent}               : make_token(adjacent,     TokenLine, TokenChars).


% identifiers and strings
{Identifier}             : make_token(tag, TokenLine, TokenChars).
\.{Identifier}           : make_token(class, TokenLine, TokenChars).
#{Identifier}            : make_token(id, TokenLine, TokenChars).
:{Identifier}            : make_token(filter, TokenLine, TokenChars).

{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).

% spaces, tabs and new lines
{Whites}                 : skip_token.

Erlang code.

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_binary(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2), Line),
    {token, {Type, Line, String}}.

unescape_string(String, Line) -> unescape_string(String, Line, []).

unescape_string([], _Line, Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Line, Output) ->
  Char = map_escaped_char(Escaped, Line),
  unescape_string(Rest, Line, [Char|Output]);
unescape_string([Char|Rest], Line, Output) ->
  unescape_string(Rest, Line, [Char|Output]).

map_escaped_char(Escaped, Line) ->
  case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $\( -> $(;
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {Line, fn_lexer, ["unrecognized escape sequence: ", [$\\, Escaped]]}})
  end.
