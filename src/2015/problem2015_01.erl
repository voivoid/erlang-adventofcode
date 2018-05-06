-module(problem2015_01).
-export([solve1/1, solve2/1]).

-spec solve1( string() ) -> integer().
solve1( Input ) ->
    lists:foldl( fun( $(, Floor ) -> Floor + 1;
                    ( $), Floor ) -> Floor - 1
                 end,
                 0,
                 Input ).

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    { _, ResultCounter } = listz:foldl_stoppable( fun( _,  { -1,    Counter } ) -> { stop, { -1, Counter } };
                                                     ( $(, { Floor, Counter } ) -> { Floor + 1, Counter + 1 };
                                                     ( $), { Floor, Counter } ) -> { Floor - 1, Counter + 1 }
                                                  end,
                                                  { 0, 0 },
                                                  stop,
                                                  Input ),
    ResultCounter.

-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 0, solve1( "(())" ) ),
      ?_assertEqual( 0, solve1( "()()" ) ),
      ?_assertEqual( 3, solve1( "(((" ) ),
      ?_assertEqual( 3, solve1( "(()(()(" ) ),
      ?_assertEqual( 3, solve1( "))(((((" ) ),
      ?_assertEqual( -1, solve1( "())" ) ),
      ?_assertEqual( -1, solve1( "))(" ) ),
      ?_assertEqual( -3, solve1( ")))" ) ),
      ?_assertEqual( -3, solve1( ")())())" ) ) ].


solve2_test_() ->
    [ ?_assertEqual( 1, solve2( ")" ) ),
      ?_assertEqual( 5, solve2( "()())" ) ) ].
