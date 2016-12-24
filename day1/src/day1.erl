-module(day1).

-compile(export_all).

%% -export([run/1
%%         ]).

-type parsed_direction() :: {atom(), pos_integer()}.
-type coordinate() :: [{atom(), integer()}].

-define(START, [{x, 0}, {y, 0}]).

%%%_* API ======================================================================

run(part_1) ->
  Input = get_input(),
  Directions = parse_directions(Input, []),
  {FinalCoordinates, _} = follow_directions(Directions, north, ?START, []),
  final_pos(FinalCoordinates);
run(part_2) ->
  Input = get_input(),
  Directions = parse_directions(Input, []),
  {_, CoordinatesAcc} = follow_directions(Directions, north, ?START, []),
  first_location_twice(CoordinatesAcc).

%% BROKEN!!!!!
first_location_twice([H|T]) ->
  lists:any(fun(Coordinate) ->
                if Coordinate =:= H ->
                    exit("Coordinates=~p BlocksFromStart=~p", [Coordinate, final_pos(Coordinate)]);
                   true -> false
                end
            end, T).

%%%_* Parse Directions =========================================================
-spec get_input() -> [string()].
get_input() ->
  {ok, FileContents} = file:read_file("priv/day1a.txt"),
  string:tokens(binary_to_list(FileContents), ",").

-spec parse_directions([string()], [parsed_direction()] | []) ->
                         [parsed_direction()].
parse_directions([], ParsedDirectionsAcc) ->
  ParsedDirectionsAcc;
parse_directions([Direction|RemainingDirections], ParsedDirectionsAcc) ->
  ParsedDirection = parse_direction(clean_input(Direction)),
  parse_directions(RemainingDirections, ParsedDirectionsAcc ++ ParsedDirection).

%% Dirty hack to clean the strings since whitespace characters are found
-spec clean_input(string()) -> string().
clean_input(Input) ->
  re:replace(Input, "\\s+", "", [global,{return,list}]).

-spec parse_direction(string()) -> [parsed_direction()].
parse_direction("R" ++ Blocks) ->
  [{right, list_to_integer(Blocks)}];
parse_direction("L" ++ Blocks) ->
  [{left, list_to_integer(Blocks)}].

%%%_* Follow Directions and Build Grid =========================================

-spec follow_directions([parsed_direction()],
                        atom(),
                        [coordinate()],
                        [coordinate()]) -> [coordinate()].
follow_directions([], _LastDirection, Coordinates, CoordinatesAcc) ->
  {Coordinates, CoordinatesAcc};
follow_directions([H|T], PrevDir, _Coordinates, CoordinatesAcc) ->
  {NewDir, {Axis, Dist}} = process_direction(H, PrevDir),
  Acc = coordinates_of_each_intersection(Axis, Dist, CoordinatesAcc),
  NewAcc = CoordinatesAcc ++ Acc,
  follow_directions(T, NewDir, lists:last(NewAcc), NewAcc).

coordinates_of_each_intersection(x, Dist, []) ->
  walk_horizontal_distance(Dist, 0, 0, []);
coordinates_of_each_intersection(y, Dist, []) ->
  walk_vertical_distance(Dist, 0, 0, []);
coordinates_of_each_intersection(x, Dist, CoordinatesAcc) ->
  [{x, Xold}, {y, Yold}] = lists:last(CoordinatesAcc),
  walk_horizontal_distance(Dist, Xold, Yold, []);
coordinates_of_each_intersection(y, Dist, CoordinatesAcc) ->
  [{x, Xold}, {y, Yold}] = lists:last(CoordinatesAcc),
  walk_vertical_distance(Dist, Xold, Yold, []).

walk_vertical_distance(0, _Xold, _Yold, Acc) ->
  Acc;
walk_vertical_distance(Blocks, Xold, Yold, Acc) when Blocks < 0 ->
  NewAcc = Acc ++ [[{x, Xold}, {y, Yold - 1}]],
  walk_vertical_distance(Blocks + 1, Xold, Yold - 1, NewAcc);
walk_vertical_distance(Blocks, Xold, Yold, Acc) when Blocks > 0 ->
  NewAcc = Acc ++ [[{x, Xold}, {y, Yold + 1}]],
  walk_vertical_distance(Blocks - 1, Xold, Yold + 1, NewAcc).

walk_horizontal_distance(0, _Xold, _Yold, Acc) ->
  Acc;
walk_horizontal_distance(Blocks, Xold, Yold, Acc) when Blocks < 0 ->
  NewAcc = Acc ++ [[{x, Xold - 1}, {y, Yold}]],
  walk_horizontal_distance(Blocks + 1, Xold - 1, Yold, NewAcc);
walk_horizontal_distance(Blocks, Xold, Yold, Acc) when Blocks > 0 ->
  NewAcc = Acc ++ [[{x, Xold + 1}, {y, Yold}]],
  walk_horizontal_distance(Blocks - 1, Xold + 1, Yold, NewAcc).

-spec process_direction({atom(), pos_integer()}, atom()) ->
                           [coordinate()].
process_direction({right, Blocks}, PrevDirection) ->
  new_direction(right, Blocks, PrevDirection);
process_direction({left, Blocks}, PrevDirection) ->
  new_direction(left, Blocks, PrevDirection).

apply_coordinates(Axis, Blocks, OldCoordinates) ->
  {Axis, OldBlocks} = proplists:lookup(Axis, OldCoordinates),
  lists:keyreplace(Axis, 1, OldCoordinates, {Axis, OldBlocks + Blocks}).

-spec new_direction(atom(), pos_integer(), atom()) -> atom().
new_direction(right, Blocks, north) ->
  {east, {x, pos(Blocks)}};
new_direction(right, Blocks, east) ->
  {south, {y, neg(Blocks)}};
new_direction(right, Blocks, south) ->
  {west, {x, neg(Blocks)}};
new_direction(right, Blocks, west) ->
  {north, {y, pos(Blocks)}};
new_direction(left, Blocks, north) ->
  {west, {x, neg(Blocks)}};
new_direction(left, Blocks, east) ->
  {north, {y, pos(Blocks)}};
new_direction(left, Blocks, south) ->
  {east, {x, pos(Blocks)}};
new_direction(left, Blocks, west) ->
  {south, {y, neg(Blocks)}}.

pos(Blocks) ->
  Blocks.
neg(Blocks) ->
  -Blocks.

-spec final_pos([coordinate()]) -> pos_integer().
final_pos(FinalCoordinates) ->
  {x, HorizontalDistance} = proplists:lookup(x, FinalCoordinates),
  {y, VerticalDistance} = proplists:lookup(y, FinalCoordinates),
  HorizontalDistance - VerticalDistance.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
