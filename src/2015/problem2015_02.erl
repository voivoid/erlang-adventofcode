-module(problem2015_02).
-export([solve1/1, solve2/1]).

-type area() :: integer().
-type dim() :: non_neg_integer().
-type calc_area_fun() :: fun( ( dim(), dim(), dim() ) -> area() ).

%%% COMMON

-spec parse_dimensions( string() ) -> { dim(), dim(), dim() }.
parse_dimensions( Line ) ->
    [ L,W,H ] = lists:map( fun erlang:list_to_integer/1, string:tokens( Line, "x" ) ),
    { L, W, H }.

-spec solve( string(), calc_area_fun() ) -> area().
solve( Input, CalcAreaFunc ) ->
    InputLines = string:tokens( Input, "\n" ),
    Sum = lists:foldl( fun( Line, Sum ) ->
                               { L, W, H } = parse_dimensions( Line ),
                               CalcAreaFunc( L, W, H ) + Sum
                       end,
                       0,
                       InputLines ),
    Sum.

%%% PART 1

-spec calc_area( dim(), dim(), dim() ) -> area().
calc_area( L, W, H ) ->
    Sides = [ L * W, W * H, H * L ],
    Area = lists:sum( lists:map( fun( X ) -> X * 2 end, Sides ) ),
    SmallestSide = lists:min( Sides ),
    Area + SmallestSide.


-spec solve1( string() ) -> integer().
solve1( Input ) ->
    solve( Input, fun calc_area/3 ).

%%% PART 2

-spec calc_ribbon( dim(), dim(), dim() ) -> area().
calc_ribbon( L, W, H ) ->
    Dims = [ L, W, H ],
    [ S1, S2, _ ] = lists:sort( Dims ),
    Wrap =  S1 * 2 + S2 * 2,
    Bow = L * W * H,
    Wrap + Bow.


-spec solve2( string() ) -> integer().
solve2( Input ) ->
    solve( Input, fun calc_ribbon/3 ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 58, solve1( "2x3x4" ) ),
      ?_assertEqual( 43, solve1( "1x1x10" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 34, solve2( "2x3x4" ) ),
      ?_assertEqual( 14, solve2( "1x1x10" ) ) ].
