%% :- module(scanner, [
%%             scan/2
%%           ]).

:- set_prolog_flag(double_quotes, chars).

% 0 or more
ws --> [W], { char_type(W, white) }, ws.
ws --> [].

nl --> [W], { char_type(W, newline) }, nl.
nl --> [].

invisible --> ws.
invisible --> nl.
invisible --> [].

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
token('!', bang).
token('=', equal).
token('<', less).
token('>', greater).
% token(_, unknown).

token(O) --> "!=", { O = bang_equal }.
token(O) --> "==", { O = equal_equal }.
token(O) --> ">=", { O = greater_equal }.
token(O) --> "<=", { O = less_equal }.
token(O) --> [I], { token(I, O) }.

is_identifier(L) :- code_type(L, alnum) ; L = '-'.

% https://www.metalevel.at/prolog/dcg
% use listing(identifier). to see what the DCG is actually making...
% 1 or more
identifier([H|T]) --> [H], { is_identifier(H) }, identifier(T).
identifier([T]) --> [T], { is_identifier(T) }.

comment(O) --> "//", { O = comment }.

any --> [W], { \+ char_type(W, newline) }, any.
any --> [].

comment_line(Result) --> comment(Result), any, nl.

lexeme(Result) --> comment_line(Result).
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
lexemes(Result) --> lexemes(state{line: 1, acc: []}, Result).
lexemes(State, Result) -->
  invisible,
  lexeme(R1),
  {
    get_dict(acc, State, Acc),
    append(Acc, [R1], R2),
    put_dict(acc, State, R2, NewState)
  },
  invisible,
  lexemes(NewState, Result).
lexemes(Result, Result) --> [].

scan(I, O) :-
  phrase(lexemes(O), I).

sample("hello world").

test(O) :-
  sample(I),
  scan(I, O).
