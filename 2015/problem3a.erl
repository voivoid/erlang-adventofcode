-module(problem3a).
-export([solve/1]).

incrementMap( Map, Coord ) ->
    maps:update_with( Coord, fun(V) -> V + 1 end, 0, Map ).

updateCoord( $^,  {X, Y} ) ->
    {X, Y-1};
updateCoord( $<,  {X, Y} ) ->
    {X-1, Y};
updateCoord( $>,  {X, Y} ) ->
    {X+1, Y};
updateCoord( $v, {X, Y} ) ->
    {X, Y+1}.

move( [], Map, _ ) ->
    maps:size( Map );
move( [Char|Input], Map, Coord ) ->
    NewCoord = updateCoord( Char, Coord ),
    move( Input, incrementMap( Map, NewCoord ), NewCoord  ).

solve(Input) ->
    move(Input, #{{0,0} => 1}, {0, 0}).
