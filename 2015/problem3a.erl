-module(problem3a).
-export([solve/1]).

incrementMap( Map, Coord ) ->
    Val = maps:get( Coord, Map, 0 ),
    maps:put( Coord, Val + 1, Map ).

updateCoord( 94,  {X, Y} ) -> % ^
    {X, Y-1};
updateCoord( 60,  {X, Y} ) -> % <
    {X-1, Y};
updateCoord( 62,  {X, Y} ) -> % >
    {X+1, Y};
updateCoord( 118, {X, Y} ) -> % v
    {X, Y+1}.

move( [], Map, _ ) ->
    maps:size( Map );
move( [Char|Input], Map, Coord ) -> % '^'
    move( Input, incrementMap( Map, Coord ), updateCoord( Char, Coord ) ).

solve(Input) ->
    move(Input, #{{0,0} => 1}, {0, 0}).
