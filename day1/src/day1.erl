-module(day1).

-compile(export_all).

-type parsed_direction() :: {atom(), pos_integer()}.
-type coordinate()       :: [{atom(), integer()}].
-type north_coordinate() :: {'north',{'y',pos_integer()}}.
-type east_coordinate()  :: {'east',{'x',pos_integer()}}.
-type south_coordinate() :: {'south',{'y',neg_integer()}}.
-type west_coordinate()  :: {'west',{'x',neg_integer()}}.

-define(START, [{x, 0}, {y, 0}]).

%%%_* API ======================================================================

-spec run(atom()) -> integer() | any().
run(part_1) ->
  Input = get_input(),
  Directions = parse_directions(Input, []),
  {FinalCoordinates, _} = follow_directions(Directions, north, ?START, []),
  final_pos(FinalCoordinates);
run(part_2) ->
  Input = get_input(),
  Directions = parse_directions(Input, []),
  {_, CoordinatesAcc} = follow_directions(Directions, north, ?START, []),
  io:format("Total number of coordinates=~p~n", [length(CoordinatesAcc)]),
  locations_visited_multiple_times(CoordinatesAcc).

%%%_* Parse Directions =========================================================
-spec get_input() -> [string()].
get_input() ->
  {ok, FileContents} = file:read_file("priv/day1a.txt"),
  string:tokens(binary_to_list(FileContents), ",").

-spec parse_directions([string()], [] | [parsed_direction()]) ->
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
-spec follow_directions([parsed_direction()] | [],
                        atom(),
                        coordinate(),
                        [coordinate()]) -> {coordinate(), [coordinate()]}.
follow_directions([], _LastDirection, Coordinates, CoordinatesAcc) ->
  {Coordinates, CoordinatesAcc};
follow_directions([H|T], PrevDir, Coordinates, CoordinatesAcc) ->
  {NewDir, {Axis, Dist}} = process_direction(H, PrevDir),
  Acc = coordinates_of_each_intersection(Axis, Dist, Coordinates),
  NewAcc = CoordinatesAcc ++ Acc,
  follow_directions(T, NewDir, lists:last(NewAcc), NewAcc).

-spec coordinates_of_each_intersection(atom(), integer(), [coordinate()]) ->
                                          [[coordinate()]].
coordinates_of_each_intersection(x, Dist, []) ->
  walk_horizontal_distance(Dist, 0, 0, []);
coordinates_of_each_intersection(y, Dist, []) ->
  walk_vertical_distance(Dist, 0, 0, []);
coordinates_of_each_intersection(x, Dist, [{x, Xold}, {y, Yold}]) ->
  walk_horizontal_distance(Dist, Xold, Yold, []);
coordinates_of_each_intersection(y, Dist, [{x, Xold}, {y, Yold}]) ->
  walk_vertical_distance(Dist, Xold, Yold, []).

-spec walk_vertical_distance(integer(),
                             integer(),
                             integer(),
                             [] | [coordinate()]) ->
                                [coordinate()].
walk_vertical_distance(0, _Xold, _Yold, Acc) ->
  Acc;
walk_vertical_distance(Blocks, Xold, Yold, Acc) when Blocks < 0 ->
  NewAcc = Acc ++ [[{x, Xold}, {y, Yold - 1}]],
  walk_vertical_distance(Blocks + 1, Xold, Yold - 1, NewAcc);
walk_vertical_distance(Blocks, Xold, Yold, Acc) when Blocks > 0 ->
  NewAcc = Acc ++ [[{x, Xold}, {y, Yold + 1}]],
  walk_vertical_distance(Blocks - 1, Xold, Yold + 1, NewAcc).

-spec walk_horizontal_distance(integer(),
                               integer(),
                               integer(),
                               [] | [coordinate()]) ->
                                  [coordinate()].
walk_horizontal_distance(0, _Xold, _Yold, Acc) ->
  Acc;
walk_horizontal_distance(Blocks, Xold, Yold, Acc) when Blocks < 0 ->
  NewAcc = Acc ++ [[{x, Xold - 1}, {y, Yold}]],
  walk_horizontal_distance(Blocks + 1, Xold - 1, Yold, NewAcc);
walk_horizontal_distance(Blocks, Xold, Yold, Acc) when Blocks > 0 ->
  NewAcc = Acc ++ [[{x, Xold + 1}, {y, Yold}]],
  walk_horizontal_distance(Blocks - 1, Xold + 1, Yold, NewAcc).

-spec process_direction({atom(), integer()}, atom()) ->
                           north_coordinate() |
                           east_coordinate()  |
                           south_coordinate() |
                           west_coordinate().
process_direction({right, Blocks}, PrevDirection) ->
  new_direction(right, Blocks, PrevDirection);
process_direction({left, Blocks}, PrevDirection) ->
  new_direction(left, Blocks, PrevDirection).

-spec apply_coordinates(atom(), integer(), [coordinate()]) -> [coordinate()].
apply_coordinates(Axis, Blocks, OldCoordinates) ->
  {Axis, OldBlocks} = proplists:lookup(Axis, OldCoordinates),
  lists:keyreplace(Axis, 1, OldCoordinates, {Axis, OldBlocks + Blocks}).

-spec new_direction(atom(), integer(), atom()) ->
                       north_coordinate() |
                       east_coordinate()  |
                       south_coordinate() |
                       west_coordinate().

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

-spec pos(integer()) -> pos_integer().
pos(Blocks) ->
  Blocks.

-spec neg(integer()) -> neg_integer().
neg(Blocks) ->
  -Blocks.

-spec locations_visited_multiple_times([coordinate()]) -> ok.
locations_visited_multiple_times([]) ->
  ok;
locations_visited_multiple_times([H|T]) ->
  location_visited_later(H, T),
  locations_visited_multiple_times(T).

-spec location_visited_later(coordinate(), [coordinate()]) -> ok.
location_visited_later(_H, []) ->
  ok;
location_visited_later(H, [H|T]) ->
  io:format("Coordinates=~p BlocksFromStart=~p~n", [H, final_pos(H)]),
  location_visited_later(H,T);
location_visited_later(H, [_|T]) ->
  location_visited_later(H,T).

-spec final_pos(coordinate()) -> integer().
final_pos(FinalCoordinates) ->
  {x, HorizontalDistance} = proplists:lookup(x, FinalCoordinates),
  {y, VerticalDistance} = proplists:lookup(y, FinalCoordinates),
  HorizontalDistance - VerticalDistance.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
