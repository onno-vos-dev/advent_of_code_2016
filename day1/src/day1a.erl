-module(day1a).

-export([run/0]).

-type parsed_direction() :: {atom(), pos_integer()}.
-type grid() :: [{atom(), integer()}].

%%%_* API ======================================================================

run() ->
  {ok, FileContents} = file:read_file("priv/day1a.txt"),
  Input = string:tokens(binary_to_list(FileContents), ","),
  ParsedDirections = parse_directions(Input, []),
  Grid = [ {north, 0}
         , {east, 0}
         , {south, 0}
         , {west, 0}
         ],
  FinalGrid = build_final_grid(ParsedDirections, north, Grid),
  final_pos(FinalGrid).

%%%_* Internal =================================================================

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

-spec build_final_grid([parsed_direction()], atom(), grid()) -> grid().
build_final_grid([], _LastDirection, Grid) ->
  Grid;
build_final_grid([H|T], PrevDir, Grid) ->
  {NewDir, Blocks} = process_direction(H, PrevDir),
  NewGrid = update_distance_in_grid({NewDir, Blocks}, Grid),
  build_final_grid(T, NewDir, NewGrid).

-spec update_distance_in_grid({atom(), pos_integer()}, grid()) -> grid().
update_distance_in_grid({NewDir, Blocks}, Grid) ->
  {NewDir, OldBlocks} = proplists:lookup(NewDir, Grid),
  lists:keyreplace(NewDir, 1, Grid, {NewDir, OldBlocks + Blocks}).

-spec process_direction({atom(), pos_integer()}, atom()) ->
                           {atom(), pos_integer()}.
process_direction({right, Blocks}, PrevDirection) ->
  NewDirection = new_direction(right, PrevDirection),
  {NewDirection, Blocks};
process_direction({left, Blocks}, PrevDirection) ->
  NewDirection = new_direction(left, PrevDirection),
  {NewDirection, Blocks}.

-spec new_direction(atom(), atom()) -> atom().
new_direction(right, north) ->
  east;
new_direction(right, east) ->
  south;
new_direction(right, south) ->
  west;
new_direction(right, west) ->
  north;
new_direction(left, north) ->
  west;
new_direction(left, east) ->
  north;
new_direction(left, south) ->
  east;
new_direction(left, west) ->
  south.

-spec final_pos(grid()) -> pos_integer().
final_pos(FinalGrid) ->
  horizontal_distance(FinalGrid) + vertical_distance(FinalGrid).

-spec horizontal_distance(grid()) -> pos_integer().
horizontal_distance(FinalGrid) ->
  {west, WestBlocks} = proplists:lookup(west, FinalGrid),
  {east, EastBlocks} = proplists:lookup(east, FinalGrid),
  if WestBlocks > EastBlocks -> WestBlocks - EastBlocks;
     WestBlocks < EastBlocks -> EastBlocks - WestBlocks;
     true -> 0
  end.

-spec vertical_distance(grid()) -> pos_integer().
vertical_distance(FinalGrid) ->
  {north, NorthBlocks} = proplists:lookup(north, FinalGrid),
  {south, SouthBlocks} = proplists:lookup(south, FinalGrid),
  if NorthBlocks > SouthBlocks -> NorthBlocks - SouthBlocks;
     NorthBlocks < SouthBlocks -> SouthBlocks - NorthBlocks;
     true -> 0
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
