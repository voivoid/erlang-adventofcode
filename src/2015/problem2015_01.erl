-module(problem2015_01).
-export([solve1/1, solve2/1]).

-type instruction() :: $( | $).
-type instructions() :: list( instruction() ).
-type floor() :: integer().
-type steps() :: non_neg_integer().

%%% PART 1

-spec count_final_floor( instructions() ) -> floor().
count_final_floor( Instructions ) ->
    lists:foldl( fun( $(, Floor ) -> Floor + 1;
                    ( $), Floor ) -> Floor - 1
                 end,
                 0,
                 Instructions ).

-spec solve1( string() ) -> integer().
solve1( Input ) ->
    count_final_floor( Input ).

%%% PART 2

-spec count_number_of_steps_till_basement( instructions() ) -> steps().
count_number_of_steps_till_basement( Instructions ) ->
    { _, Steps } = listz:foldl_stoppable( fun( _,  { -1,    Steps } ) -> { stop, { -1, Steps } };
                                             ( $(, { Floor, Steps } ) -> { Floor + 1, Steps + 1 };
                                             ( $), { Floor, Steps } ) -> { Floor - 1, Steps + 1 }
                                          end,
                                          { 0, 0 },
                                          stop,
                                          Instructions ),
    Steps.

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    count_number_of_steps_till_basement( Input ).

%%% TESTS

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
