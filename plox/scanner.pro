%% :- module(scanner, [
%%             scan/2
%%           ]).

:- set_prolog_flag(double_quotes, chars).

% 0 or more
ws --> [W], { char_type(W, space) }, ws.
ws --> [].

keyword(O) --> "and"    , { O = and }.
keyword(O) --> "class"  , { O = class }.
keyword(O) --> "else"   , { O = else }.
keyword(O) --> "false"  , { O = false }.
keyword(O) --> "for"    , { O = for }.
keyword(O) --> "fun"    , { O = fun }.
keyword(O) --> "if"     , { O = if }.
keyword(O) --> "nil"    , { O = nil }.
keyword(O) --> "or"     , { O = or }.
keyword(O) --> "print"  , { O = print }.
keyword(O) --> "return" , { O = return }.
keyword(O) --> "super"  , { O = super }.
keyword(O) --> "this"   , { O = this }.
keyword(O) --> "true"   , { O = true }.
keyword(O) --> "var"    , { O = var }.
keyword(O) --> "while"  , { O = while }.

token('(', paren_left).
token(')', paren_right).
token('{', brace_left).
token('}', brace_right).
token('[', bracket_left).
token(']', bracket_right).
% token(_, unknown).

token(O) --> [I], { token(I, O) }.

is_identifier(L) :- code_type(L, alnum) ; L = '-'.

% https://www.metalevel.at/prolog/dcg
% use listing(identifier). to see what the DCG is actually making...
% 1 or more
identifier([H|T]) --> [H], { is_identifier(H) }, identifier(T).
identifier([T]) --> [T], { is_identifier(T) }.

lexeme(Result) --> token(Result).
lexeme(Result) --> keyword(Result).
lexeme(Result) -->
  \+ keyword(_),
  identifier(Letters),
  {
    string_chars(Word, Letters),
    Result = id{ word: Word }
  }.
% phrase(lexeme(N), "{"). -> N = brace_left.

lexemes([]) --> [].
lexemes(Result) --> lexemes([], Result).
lexemes(Acc, Result) --> ws, lexeme(R1), { append(Acc, [R1], R2) }, ws, lexemes(R2, Result).
lexemes(Result, Result) --> [].

scan(I, O) :-
  phrase(lexemes(O), I).

sample("hello world").

test(O) :-
  sample(I),
  scan(I, O).
