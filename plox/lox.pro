% TODO Add scanner to create tokens

run(In) :-
  format("~w~n", [In]).

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
