% Ensure that .pro files will be loaded for prolog calls
% http://www.swi-prolog.org/pldoc/man?predicate=prolog_file_type%2f2
user:prolog_file_type(pro, prolog).

% TODO Add scanner to create tokens

:- use_module(scanner).

run(In) :-
  scanner:scan(In, Out),
  format("~w~n", [Out]).

run_prompt() :-
  read_line_to_codes(user_input, Cs),
  Cs \= end_of_file,
  atom_codes(A, Cs),
  atomic_list_concat(L, ' ', A),
  run(A),
  run_prompt.

run_prompt() :- format("bye~n").

main() :-
  format("Enter some input:"),
  run_prompt.
