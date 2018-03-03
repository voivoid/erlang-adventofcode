-module(problem2015_02).
-export([solve1/1, solve2/1]).

-spec calc_area( string() ) -> integer().
calc_area( Input ) ->
    [ L,W,H ] = lists:map( fun erlang:list_to_integer/1, string:tokens( Input, "x" ) ),
    Sides = [ L * W, W * H, H * L ],
    Area = lists:sum( lists:map( fun( X ) -> X * 2 end, Sides ) ),
    SmallestSide = lists:min( Sides ),
    Area + SmallestSide.

-spec solve1( string() ) -> integer().
solve1( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    Sum = lists:foldl( fun( Line, Sum ) -> R = calc_area( Line ), R + Sum end, 0, Lines ),
    Sum.
                            
    
-spec calc_ribbon( string() ) -> integer().
calc_ribbon( Input ) ->
    Dims = lists:map( fun erlang:list_to_integer/1, string:tokens( Input, "x" ) ),
    [ L,W,H ] = Dims,
    [ S1, S2, _ ] = lists:sort( Dims ),
    Wrap =  S1 * 2 + S2 * 2,
    Bow = L * W * H,
    Wrap + Bow.

-spec solve2( string() ) -> integer().
solve2( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    Sum = lists:foldl( fun( Line, Sum ) -> R = calc_ribbon( Line ), R + Sum end, 0, Lines ),
    Sum.
                            
    


-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 58, solve1( "2x3x4" ) ),
      ?_assertEqual( 43, solve1( "1x1x10" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 34, solve2( "2x3x4" ) ),
      ?_assertEqual( 14, solve2( "1x1x10" ) ) ].
