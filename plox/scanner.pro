%% :- module(scanner, [
%%             scan/2
%%           ]).

:- set_prolog_flag(double_quotes, chars).

% 0 or more
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
% 1 or more
identifier([H|T]) --> [H], { code_type(H, alnum) ; H = '-' }, identifier(T).
identifier([T]) --> [T], { code_type(T, alnum) ; T = '-' }.

lexeme(Result) --> token(Result). % ; token(R1), Result = R1, write(R1).
lexeme(Result) --> identifier(Result). % ; token(R1), Result = R1, write(R1).
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
