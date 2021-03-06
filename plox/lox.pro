% Ensure that .pro files will be loaded for prolog calls
% http://www.swi-prolog.org/pldoc/man?predicate=prolog_file_type%2f2
:- set_prolog_flag(double_quotes, chars).

user:prolog_file_type(pro, prolog).

% TODO Add scanner to create tokens

:- use_module(scanner).
:- use_module(parser).

run(In) :-
  scanner:scan(In, state{acc: Acc, err:[], line: _}),
  parser:parse(Acc, Parsed),
  parser:pp(Parsed), format("~n").

run(In) :-
  scanner:scan(In, state{acc: _, err:Err, line: _}),
  format("Couldn't parse something: ~w~n", [Err]).

run_prompt() :-
  read_line_to_string(user_input, Line),
  Line \= end_of_file,
  string_chars(Line, Cs),
  run(Cs),
  run_prompt.

run_prompt() :- format("bye~n").

main() :-
  current_prolog_flag(argv, Argv),
  nth0(0, Argv, Argument0), % get first argument
  scanner:scan_file(Argument0, Out),
  format("~w~n", [Out]).

main() :-
  format("Enter some input:"),
  run_prompt.
