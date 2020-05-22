% Ensure that .pro files will be loaded for prolog calls
% http://www.swi-prolog.org/pldoc/man?predicate=prolog_file_type%2f2
:- set_prolog_flag(double_quotes, chars).

user:prolog_file_type(pro, prolog).

% TODO Add scanner to create tokens

:- use_module(scanner).

test() :-
  scanner:scan("var x = 5 + 10;", R),
  format("~w", [R]).

main() :-
  test.
