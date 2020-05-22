:- module(scanner, [
            scan/2
          ]).

:- use_module(library(pio)).

:- set_prolog_flag(double_quotes, chars).

make_error(ErrorMessage, Unknown, LineNo) :-
  integer(Unknown),
  format(string(ErrorMessage), "Unrecognized ~c on line: ~w", [Unknown, LineNo]).

make_error(ErrorMessage, Unknown, LineNo) :-
  format(string(ErrorMessage), "Unrecognized ~w on line: ~w", [Unknown, LineNo]).

% Append a new item to a dict key
append_dict(Key, D1, D2, Val) :-
  get_dict(Key, D1, Acc),
  append(Acc, [Val], NewAcc),
  put_dict(Key, D1, NewAcc, D2).

append_acc(D1, D2, Val) :- append_dict(acc, D1, D2, Val).
append_err(D1, D2, Val) :- append_dict(err, D1, D2, Val).

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

% When parsing the grammer from file, char code isn't translated always...
token(O) --> [I], { integer(I), char_code(C, I), token(C, O) }.

is_identifier(L) :- code_type(L, alnum) ; L = '-'.

% Some literal matches
double_quote --> ['"'].
double_quote --> [I], { integer(I), char_code(C, I), C = '"' }.

dot --> ['.'].
dot --> [I], { char_code(C, I), C = '.' }.

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
  { append_acc(NewState2, NewState3, R1) },
  invisible(NewState3, NewState4),
  lexemes(NewState4, Result).

% Error branch
lexemes(State, Result) -->
  [Unknown],
  invisible(State, NewState1),
  {
    get_dict(line, NewState1, LineNo),
    make_error(ErrorMessage, Unknown, LineNo),
    append_err(NewState1, NewState2, ErrorMessage)
  },
  invisible(NewState2, NewState3),
  lexemes(NewState3, Result).

lexemes(Result, Result) --> [].

scan(I, O) :-
  phrase(lexemes(O), I).

scan_file(I, O) :-
  phrase_from_file(lexemes(O), I).

sample("hello world").

test(O) :-
  sample(I),
  scan(I, O).
