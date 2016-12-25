-module(day2).

-compile(export_all).

%% -export([run/1
%%         ]).

%%%_* API ======================================================================

run(part_1) ->
  Input = get_input(),
  calculate_code(Input, 5, []);
run(part_2) ->
  ok.

%%%_* Helpers ==================================================================
-spec get_input() -> [string()].
get_input() ->
  {ok, FileContents} = file:read_file("priv/day2.txt"),
  string:tokens(binary_to_list(FileContents), "$\n").

calculate_code([], _Digit, Acc) ->
  Acc;
calculate_code([H|T], Digit, Acc) ->
  NewDigit = follow_instructions(H, Digit),
  calculate_code(T, NewDigit, Acc ++ integer_to_list(NewDigit)).

follow_instructions([], Digit) ->
  Digit;
follow_instructions([H|T], Digit) ->
  NewDigit = follow_instruction(H, Digit),
  follow_instructions(T, NewDigit).

follow_instruction($U, 1) ->
  1;
follow_instruction($U, 2) ->
  2;
follow_instruction($U, 3) ->
  3;
follow_instruction($U, 4) ->
  1;
follow_instruction($U, 5) ->
  2;
follow_instruction($U, 6) ->
  3;
follow_instruction($U, 7) ->
  4;
follow_instruction($U, 8) ->
  5;
follow_instruction($U, 9) ->
  6;
follow_instruction($D, 1) ->
  4;
follow_instruction($D, 2) ->
  5;
follow_instruction($D, 3) ->
  6;
follow_instruction($D, 4) ->
  7;
follow_instruction($D, 5) ->
  8;
follow_instruction($D, 6) ->
  9;
follow_instruction($D, 7) ->
  7;
follow_instruction($D, 8) ->
  8;
follow_instruction($D, 9) ->
  9;
follow_instruction($L, 1) ->
  1;
follow_instruction($L, 2) ->
  1;
follow_instruction($L, 3) ->
  2;
follow_instruction($L, 4) ->
  4;
follow_instruction($L, 5) ->
  4;
follow_instruction($L, 6) ->
  5;
follow_instruction($L, 7) ->
  7;
follow_instruction($L, 8) ->
  7;
follow_instruction($L, 9) ->
  8;
follow_instruction($R, 1) ->
  2;
follow_instruction($R, 2) ->
  3;
follow_instruction($R, 3) ->
  3;
follow_instruction($R, 4) ->
  5;
follow_instruction($R, 5) ->
  6;
follow_instruction($R, 6) ->
  6;
follow_instruction($R, 7) ->
  8;
follow_instruction($R, 8) ->
  9;
follow_instruction($R, 9) ->
  9.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
