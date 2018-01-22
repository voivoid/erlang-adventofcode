-module(problem3b).
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

move( [], Map, _, _ ) ->
    maps:size( Map );
move( [Char|Input], Map, Coord1, Coord2 ) -> % '^'
    NewCoord = updateCoord( Char, Coord1 ),
    move( Input, incrementMap( Map, NewCoord ), Coord2, NewCoord ).

solve(Input) ->
    move(Input, #{{0,0} => 2}, {0, 0}, {0, 0} ).
