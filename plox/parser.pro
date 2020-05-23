%% :- module(parser, [
%%             parse/2
%%           ]).

:- use_module(library(pio)).

:- set_prolog_flag(double_quotes, chars).

%% BNF - Rules to avoid left recursion

%% expression     → equality ;
%% equality       → comparison ( ( "!=" | "==" ) comparison )* ;
%% comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
%% addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
%% multiplication → unary ( ( "/" | "*" ) unary )* ;
%% unary          → ( "!" | "-" ) unary
%%                | primary ;
%% primary        → NUMBER | STRING | "false" | "true" | "nil"
%%                | "(" expression ")" ;

%% S = State
number        --> [number].
string        --> [string].
bang          --> [bang].
minus         --> [minus].
star          --> [star].
slash         --> [slash].
plus          --> [plus].
minus         --> [minus].
greater       --> [greater].
greater_equal --> [greater_equal].
less          --> [less].
less_equal    --> [less_equal].
bang_equal    --> [bang_equal].
equal_equal   --> [equal_equal].

primary --> number ; string.

unary --> (bang ; minus), unary.
unary --> primary.

some_unary --> (slash ; star), unary, some_unary.
some_unary --> [].

multiplication --> unary, some_unary.

some_multiplication --> (minus ; plus), multiplication, some_multiplication.
some_multiplication --> [].

addition --> multiplication, some_multiplication.

some_addition --> (greater ; greater_equal ; less ; less_equal), addition, some_addition.
some_addition --> [].

comparison --> addition, some_addition.

some_comparison --> (bang_equal ; equal_equal), comparison, some_comparison.
some_comparison --> [].

equality --> comparison, some_comparison.

expression --> equality.

%expression(S) --> equality(S).
%equality(S) --> comparison
