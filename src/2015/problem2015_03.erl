-module(problem2015_03).
-export([solve1/1, solve2/1]).

-type chars() :: $^ | $< | $> | $v.
-type coord() :: { integer(), integer() }.
-type coord_map() :: #{ coord() := non_neg_interger }.

-spec increment_map( coord_map(), coord() ) -> coord_map().
increment_map( Map, Coord ) ->
    maps:update_with( Coord, fun(V) -> V + 1 end, 0, Map ).

-spec update_coord( chars(), coord() ) -> coord().
update_coord( $^,  {X, Y} ) ->
    {X, Y-1};
update_coord( $<,  {X, Y} ) ->
    {X-1, Y};
update_coord( $>,  {X, Y} ) ->
    {X+1, Y};
update_coord( $v, {X, Y} ) ->
    {X, Y+1}.


-spec move1( [ chars() ], coord_map(), coord() ) -> non_neg_integer().
move1( [], Map, _ ) ->
    maps:size( Map );
move1( [Char|Input], Map, Coord ) ->
    NewCoord = update_coord( Char, Coord ),
    move1( Input, increment_map( Map, NewCoord ), NewCoord  ).

-spec solve1( [ chars() ] ) -> non_neg_integer().
solve1(Input) ->
    move1(Input, #{{0,0} => 1}, {0, 0}).


-spec move2( [ chars() ], coord_map(), coord(), coord() ) -> non_neg_integer().
move2( [], Map, _, _ ) ->
    maps:size( Map );
move2( [Char|Input], Map, Coord1, Coord2 ) -> % '^'
    NewCoord = update_coord( Char, Coord1 ),
    move2( Input, increment_map( Map, NewCoord ), Coord2, NewCoord ).

-spec solve2( [ chars() ] ) -> non_neg_integer().
solve2(Input) ->
    move2(Input, #{{0,0} => 2}, {0, 0}, {0, 0} ).
