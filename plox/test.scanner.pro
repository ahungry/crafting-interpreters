% Ensure that .pro files will be loaded for prolog calls
% http://www.swi-prolog.org/pldoc/man?predicate=prolog_file_type%2f2
:- set_prolog_flag(double_quotes, chars).

user:prolog_file_type(pro, prolog).

% TODO Add scanner to create tokens

:- use_module(scanner).

test(In) :-
  string_chars(In, Cs),
  scanner:scan(Cs, R),
  format("~w", [R]).

main(In) :-
  test(In).
