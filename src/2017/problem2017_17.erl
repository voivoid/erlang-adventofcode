-module(problem2017_17).
-export([solve1/1, solve2/1]).

-type stepping() :: non_neg_integer().
-type pos() :: non_neg_integer().
-type value() :: non_neg_integer().
-type values() :: zipper:zipper( value() ).
-type counter() :: non_neg_integer().

-spec run_steps( stepping(), values(), counter(), counter() ) -> values().
run_steps( _, Values, Counter, Counter ) ->
    Values;
run_steps( Stepping, Values, StopN, Counter ) ->
    UpdatedValues = zipper:next( zipper:append( Counter, zipper:next_n( Stepping, Values ) ) ),
    run_steps( Stepping, UpdatedValues, StopN, Counter + 1 ).

-spec solve1( string() ) -> value().
solve1( Input ) ->
    Stepping = erlang:list_to_integer( Input ),
    Zipper = run_steps( Stepping, zipper:from_list( [ 0 ] ), 2018, 1 ),
    zipper:get( zipper:next( Zipper ) ).

-spec count_steps( stepping(), pos(), counter(), counter(), value() ) -> value().
count_steps( _, _, Counter, Counter, LastValAfter0 ) ->
    LastValAfter0;
count_steps( Stepping, Pos, StopN, Counter, LastValAfter0 ) ->
    NewPos = 1 + ( Pos + Stepping ) rem Counter,
    NewLastValAfter0 =
        case NewPos of
            1 -> Counter;
            _ -> LastValAfter0
        end,
    count_steps( Stepping, NewPos, StopN, Counter + 1, NewLastValAfter0 ).

-spec solve2( string() ) -> value().
solve2( Input ) ->
    Stepping = erlang:list_to_integer( Input ),
    count_steps( Stepping, 0, 50000000, 1, 0 ).

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 638, solve1( "3" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 0, count_steps( 3, 0, 1, 1, 0 ) ),
      ?_assertEqual( 1, count_steps( 3, 0, 2, 1, 0 ) ),
      ?_assertEqual( 2, count_steps( 3, 0, 3, 1, 0 ) ),
      ?_assertEqual( 2, count_steps( 3, 0, 4, 1, 0 ) ),
      ?_assertEqual( 2, count_steps( 3, 0, 5, 1, 0 ) ),
      ?_assertEqual( 5, count_steps( 3, 0, 6, 1, 0 ) ),
      ?_assertEqual( 5, count_steps( 3, 0, 7, 1, 0 ) ),
      ?_assertEqual( 5, count_steps( 3, 0, 8, 1, 0 ) ),
      ?_assertEqual( 5, count_steps( 3, 0, 9, 1, 0 ) ),
      ?_assertEqual( 9, count_steps( 3, 0, 10, 1, 0 ) )
    ].
