%% :- module(scanner, [
%%             scan/2
%%           ]).

:- set_prolog_flag(double_quotes, chars).

ws --> [W], { char_type(W, white) }.

nl(State, NewState) --> [W], {
                          char_type(W, newline),
                          get_dict(line, State, Line),
                          Count is Line + 1,
                          put_dict(line, State, Count, NewState)
                        }.

invisible(State, NewState) --> nl(State, ImState1), invisible(ImState1, NewState).
invisible(State, NewState) --> ws, invisible(State, NewState).
invisible(State, State) --> [].

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

token(',', comma).
token('.', dot).

token('-', minus).
token('+', plus).
token(';', semicolon).
token('*', star).
token('/', slash).

token('!', bang).
token('=', equal).
token('<', less).
token('>', greater).

token(O) --> "!=", { O = bang_equal }.
token(O) --> "==", { O = equal_equal }.
token(O) --> ">=", { O = greater_equal }.
token(O) --> "<=", { O = less_equal }.
token(O) --> [I], { token(I, O) }.

is_identifier(L) :- code_type(L, alnum) ; L = '-'.

% Some literal matches
double_quote --> ['"'].
dot --> ['.'].

% Numeric stuff
is_digit(L) :- code_type(L, digit).

digits([H|T]) --> [H], { is_digit(H) }, digits(T).
digits([T]) --> [T], { is_digit(T) }.

float(Result) --> digits(Digits1), dot, digits(Digits2),
                  {
                    append(Digits1, ['.'], X),
                    append(X, Digits2, Y),
                    string_chars(Value, Y),
                    Result = number{ val: Value }
                  }.
integer(Result) --> digits(Digits),
                    {
                      string_chars(Value, Digits),
                      Result = number{ val: Value }
                    }.

number(Result) --> integer(Result).
number(Result) --> float(Result).

% https://www.metalevel.at/prolog/dcg
% use listing(identifier). to see what the DCG is actually making...
% 1 or more
identifier([H|T]) --> [H], { is_identifier(H) }, identifier(T).
identifier([T]) --> [T], { is_identifier(T) }.

comment(O) --> "//", { O = comment }.

any --> [W], { \+ char_type(W, newline) }, any.
any --> [].

comment_line(State, NewState, Result) --> comment(Result), any, invisible(State, NewState).

% TODO: Capture the string values
str_line(State, State, string) --> double_quote, any, double_quote.

lexeme(State, NewState, Result) --> comment_line(State, NewState, Result).
lexeme(State, NewState, Result) --> str_line(State, NewState, Result).
lexeme(State, State, Result) --> token(Result).
lexeme(State, State, Result) --> keyword(Result).
lexeme(State, State, Result) --> number(Result).
lexeme(State, State, Result) -->
  %\+ keyword(_),
  identifier(Letters),
  {
    string_chars(Value, Letters),
    Result = id{ val: Value }
  }.
% phrase(lexeme(N), "{"). -> N = brace_left.

lexemes([]) --> [].
lexemes(Result) --> lexemes(state{line: 1, acc: [], err: []}, Result).
lexemes(State, Result) -->
  invisible(State, NewState1),
  lexeme(NewState1, NewState2, R1),
  {
    get_dict(acc, NewState2, Acc),
    append(Acc, [R1], R2),
    put_dict(acc, NewState2, R2, NewState3)
  },
  invisible(NewState3, NewState4),
  lexemes(NewState4, Result).

% Error branch
lexemes(State, Result) -->
  [Unknown],
  invisible(State, NewState1),
  {
    get_dict(err, NewState1, Err),
    get_dict(line, NewState1, LineNo),
    format(string(ErrorMessage), "Unrecognized ~w on line: ~w", [Unknown, LineNo]),
    append(Err, [ErrorMessage], NewErr),
    put_dict(err, NewState1, NewErr, NewState2)
  },
  invisible(NewState2, NewState3),
  lexemes(NewState3, Result).

lexemes(Result, Result) --> [].

scan(I, O) :-
  phrase(lexemes(O), I).

sample("hello world").

test(O) :-
  sample(I),
  scan(I, O).
