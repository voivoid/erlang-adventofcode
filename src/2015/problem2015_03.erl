-module(problem2015_03).
-export([solve1/1, solve2/1]).

-type move() :: $^ | $< | $> | $v.
-type moves() :: [ move() ].
-type coord() :: { integer(), integer() }.
-type visited_houses() :: sets:set( coord() ).
-type visitation_strategy() :: fun( ( string(), coord(), visited_houses() ) -> visited_houses() ).

%%% COMMON

-spec get_number_of_visited_houses( visited_houses() ) -> non_neg_integer().
get_number_of_visited_houses( VisitedHouses ) ->
    sets:size( VisitedHouses ).

-spec visit_house( coord(), visited_houses() ) -> visited_houses().
visit_house( Coord, VisitedHouses ) ->
    sets:add_element( Coord, VisitedHouses ).

-spec no_visited_houses() -> visited_houses().
no_visited_houses() ->
    sets:new().

-spec next_coord( move(), coord() ) -> coord().
next_coord( $^, { X, Y } ) ->
    { X, Y - 1 };
next_coord( $<, { X, Y } ) ->
    { X - 1, Y };
next_coord( $>, { X, Y } ) ->
    { X + 1, Y };
next_coord( $v, { X, Y } ) ->
    { X, Y + 1 }.

-spec move_santa( move(), { coord(), visited_houses() } ) -> { coord(), visited_houses() }.
move_santa( Move, { SantaCoord, VisitedHouses } ) ->
    NextCoord = next_coord( Move, SantaCoord ),
    UpdatedVisitedHouses = visit_house( NextCoord, VisitedHouses ),
    { NextCoord, UpdatedVisitedHouses }.

-spec solve( moves(), visitation_strategy() ) -> non_neg_integer().
solve( Input, VisitStrategy ) ->
    InitialCoord = { 0, 0 },
    InitialVisitedHouses = visit_house( InitialCoord, no_visited_houses() ),

    TotalVisitedHouses = VisitStrategy( Input, InitialCoord, InitialVisitedHouses ),
    get_number_of_visited_houses( TotalVisitedHouses ).


%%% PART 1

-spec visit_strategy1( string(), coord(), visited_houses() ) -> visited_houses().
visit_strategy1( Input, InitialCoord, InitialVisitedHouses ) ->
    { _, TotalVisitedHouses } =
        lists:foldl( fun move_santa/2,
                     { InitialCoord, InitialVisitedHouses },
                     Input ),
    TotalVisitedHouses.

-spec solve1( [ move() ] ) -> non_neg_integer().
solve1( Input ) ->
    solve( Input, fun visit_strategy1/3 ).

%%% PART 2

-spec visit_strategy2( string(), coord(), visited_houses() ) -> visited_houses().
visit_strategy2( Input, InitialCoord, InitialVisitedHouses ) ->
    { _, _, TotalVisitedHouses } =
        lists:foldl( fun( Move, { CoordToMove, CoordToSkip, VisitedHouses } ) ->
                             { NextCoord, UpdatedVisitedHouses } = move_santa( Move, { CoordToMove, VisitedHouses } ),
                             { CoordToSkip, NextCoord, UpdatedVisitedHouses }
                     end,
                     { InitialCoord, InitialCoord, InitialVisitedHouses },
                     Input ),
    TotalVisitedHouses.

-spec solve2( [ move() ] ) -> non_neg_integer().
solve2( Input ) ->
    solve( Input, fun visit_strategy2/3 ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 2, solve1( ">" ) ),
      ?_assertEqual( 4, solve1( "^>v<" ) ),
      ?_assertEqual( 2, solve1( "^v^v^v^v^v" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 3, solve2( "^v" ) ),
      ?_assertEqual( 3, solve2( "^>v<" ) ),
      ?_assertEqual( 11, solve2( "^v^v^v^v^v" ) ) ].
