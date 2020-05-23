:- module(parser, [
            parse/2
          ]).

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
number(R)        --> [number{val: N}], { R = number{val: N} }.
id(R)            --> [id{val: S}],     { R = id{val: S} }.
string(R)        --> [string{val: S}], { R = string{val: S} }.
string(R)        --> [string],         { R = string{val: "?"} }.
bang(R)          --> [bang],           { R = bang }.
minus(R)         --> [minus],          { R = minus }.
star(R)          --> [star],           { R = star }.
slash(R)         --> [slash],          { R = slash }.
plus(R)          --> [plus],           { R = plus }.
greater(R)       --> [greater],        { R = greater }.
greater_equal(R) --> [greater_equal],  { R = greater_equal }.
less(R)          --> [less],           { R = less }.
less_equal(R)    --> [less_equal],     { R = less_equal }.
bang_equal(R)    --> [bang_equal],     { R = bang_equal }.
equal_equal(R)   --> [equal_equal],    { R = equal_equal }.

primary(R) --> number(R) ; string(R).

unary(R) --> (bang(Op) ; minus(Op)), unary(Right), { R = unary{op: Op, right: Right}}.
unary(R) --> primary(R).

some_unary(Expr, R) --> (slash(Op) ; star(Op)), unary(Right),
                        { NewExpr = binary{op: Op, left: Expr, right: Right} },
                        some_unary(NewExpr, R).
some_unary(R, R) --> [].

multiplication(R) --> unary(Expr), some_unary(Expr, R).

some_multiplication(Expr, R) --> (minus(Op) ; plus(Op)), multiplication(Right),
                        { NewExpr = binary{op: Op, left: Expr, right: Right }},
                        some_multiplication(NewExpr, R).
some_multiplication(R, R) --> [].

addition(R) --> multiplication(Expr), some_multiplication(Expr, R).

some_addition(Expr, R) --> (greater(Op) ; greater_equal(Op) ; less(Op) ; less_equal(Op)),
                           addition(Right),
                           { NewExpr = binary{op: Op, left: Expr, right: Right }},
                           some_addition(NewExpr, R).
some_addition(R, R) --> [].

comparison(R) --> addition(Expr), some_addition(Expr, R).

some_comparison(Expr, R) --> (bang_equal(Op) ; equal_equal(Op)), comparison(Right),
                             { NewExpr = binary{op: Op, left: Expr, right: Right }},
                             some_comparison(NewExpr, R).
some_comparison(R, R) --> [].

equality(R) --> comparison(Expr), some_comparison(Expr, R).

expression(R) --> equality(R).

%expression(S) --> equality(S).
%equality(S) --> comparison

pp(binary{op: Op, left: Left, right: Right}) :-
  format(" ("),
  format("~w", [Op]),
  pp(Left), pp(Right),
  format(")").
pp(unary{op: Op, right: Right}) :-
  format(" ("),
  format("~w", [Op]),
  pp(Right),
  format(")").
pp(number{val: Val}) :- format(" ~w", [Val]).
pp(string{val: Val}) :- string_chars(S, Val), format(" \"~w\"", [S]).

parse(In, Out) :- phrase(expression(Out), In).

test_pp() :-
  phrase(expression(R), [minus, number{val: 8}, plus, string{val: "yaya"}]),
  format("~w~n", R),
  pp(R).
