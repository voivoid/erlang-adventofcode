-module(problem2016_13).
-export([solve1/1]).

-type favnum() :: integer().
-type coord() :: non_neg_integer().
-type pos() :: { coord(), coord() }.
-type visited() :: sets:set( pos() ).
-type path_len() :: non_neg_integer().
-type to_visit() :: queue:queue( pos(), path_len() ).

-spec coord_neighbours( coord() ) -> coord().
coord_neighbours( 0 ) -> [ 1 ];
coord_neighbours( C ) -> [ C - 1, C + 1 ].

-spec is_movable( favnum(), pos() ) -> boolean().
is_movable( FavNum, { X, Y } ) ->
    Sum = FavNum + ( X * X + 3 * X + 2 * X * Y + Y + Y * Y ),
    NumOfOneBits = erlang:length( [ Bit || <<Bit:1>> <= <<Sum:32>>, Bit == 1 ] ),
    NumOfOneBits rem 2 == 0.

-spec get_neighbours( pos() ) -> [ pos() ].
get_neighbours( { X, Y } ) ->
    [ { NX, Y } || NX <- coord_neighbours( X ) ] ++
    [ { X, NY } || NY <- coord_neighbours( Y ) ].

-spec get_movable_neighbours( favnum(), pos() ) -> [ pos() ].
get_movable_neighbours( FavNum, Pos ) ->
    Neighbours = get_neighbours( Pos ),
    lists:filter( fun( Neighbour ) -> is_movable( FavNum, Neighbour ) end, Neighbours ).

-spec find_shortest_path_len( favnum(), pos(), to_visit(), visited() ) -> path_len().
find_shortest_path_len( FavNum, FinishPos, ToVisit, Visited ) ->
    { { value, { CurrentPos, CurrentPathLen } }, ToVisitWithoutCurrent } = queue:out( ToVisit ),
    case { FinishPos == CurrentPos, sets:is_element( CurrentPos, Visited ) } of
        { true, _ } -> CurrentPathLen;
        { _, true } -> find_shortest_path_len( FavNum, FinishPos, queue:drop( ToVisit ) , Visited );
        _ ->
            CurrentNeighbours = get_movable_neighbours( FavNum, CurrentPos ),
            NewToVisit = lists:foldl( fun( ToVisitPos, ToVisitAcc ) ->
                                              queue:in( { ToVisitPos, CurrentPathLen + 1 }, ToVisitAcc )
                                      end,
                                      ToVisitWithoutCurrent,
                                      CurrentNeighbours ),
            NewVisited = sets:add_element( CurrentPos, Visited ),
            find_shortest_path_len( FavNum, FinishPos, NewToVisit, NewVisited )
    end.

-spec solve1( string(), integer() ) -> non_neg_integer().
solve1( Input, FinishPos ) ->
    FavNum = erlang:list_to_integer( Input ),
    StartPos = { 1, 1 },
    find_shortest_path_len( FavNum, FinishPos, queue:from_list( [ { StartPos, 0 } ] ), sets:new() ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve1( Input, { 31, 39 } ).

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( [ { 1, 0 }, { 0, 1 } ], get_neighbours( { 0, 0 } ) ),
      ?_assertEqual( [ { 2, 3 }, { 4, 3 }, { 3, 2 }, { 3, 4 }  ], get_neighbours( { 3, 3 } ) ),
      ?_assert( is_movable( 10, { 0, 0 } ) ),
      ?_assert( is_movable( 10, { 1, 1 } ) ),
      ?_assert( is_movable( 10, { 0, 1 } ) ),
      ?_assert( not is_movable( 10, { 1, 0 } ) ),
      ?_assert( not is_movable( 10, { 2, 1 } ) ),
      ?_assert( not is_movable( 10, { 0, 2 } ) ),
      ?_assertEqual( 0, solve1( "10", { 1, 1 } ) ),
      ?_assertEqual( 1, solve1( "10", { 1, 2 } ) ),
      ?_assertEqual( 2, solve1( "10", { 2, 2 } ) ),
      ?_assertEqual( 11, solve1( "10", { 7, 4 } ) )
    ].
