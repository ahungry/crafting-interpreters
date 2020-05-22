%% :- module(scanner, [
%%             scan/2
%%           ]).

:- set_prolog_flag(double_quotes, chars).

ws --> [W], { char_type(W, space) }, ws.
ws --> [].

token('(', paren_left).
token(')', paren_right).
token('{', brace_left).
token('}', brace_right).
% token(_, unknown).

token(O) --> [I], { token(I, O) }.

% https://www.metalevel.at/prolog/dcg
% use listing(identifier). to see what the DCG is actually making...
identifier([H|T]) --> [H], { code_type(H, alpha) ; H = '-' }, identifier(T).
identifier([T]) --> [T].

lexeme(Result) --> identifier(Result) ; token(Result).
% phrase(lexeme(N), "{"). -> N = brace_left.

lexemes([]) --> [].
lexemes(Result) --> lexemes([], Result).
lexemes(Acc, Result) --> lexeme(R1), { append(Acc, [R1], R2) }, lexemes(R2, Result).
lexemes(Result, Result) --> [].

scan(I, O) :-
  phrase(lexemes(O), I).

sample("hello world").

test(O) :-
  sample(I),
  scan(I, O).
