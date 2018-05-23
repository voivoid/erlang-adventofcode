-module(problem2015_12).
-export([solve1/1, solve2/1]).

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().

sum_all_numbers( Input ) ->
    Separators = lists:seq( $a, $z ) ++ "{}[],\":",
    NumbersStrs = string:tokens( Input, Separators ),
    Numbers = lists:map( fun erlang:list_to_integer/1, NumbersStrs ),
    lists:sum( Numbers ).

solve1( Input ) ->
    sum_all_numbers( Input ).

%%% PART 2

-spec sum_all_numbers_except_red( jiffy:json_value() ) -> non_neg_integer().
sum_all_numbers_except_red( Bin ) when erlang:is_binary( Bin ) ->
    0;
sum_all_numbers_except_red( Int ) when erlang:is_integer( Int ) ->
    Int;
sum_all_numbers_except_red( List ) when erlang:is_list( List ) ->
    lists:foldl( fun( E, Acc ) ->
                         Acc + sum_all_numbers_except_red( E )
                 end,
                 0,
                 List );
sum_all_numbers_except_red( Map ) when erlang:is_map( Map ) ->
    { Counter, _ } = maps:fold( fun ( _, _,         { _, true } = Acc ) -> Acc;
                                    ( _, <<"red">>, _                 ) -> { 0, true };
                                    ( _, V,         { Counter, S }    ) -> { Counter + sum_all_numbers_except_red( V ), S }
                                end,
                                { 0, false },
                                Map ),
    Counter.

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    Json = jiffy:decode( erlang:list_to_binary( Input ), [ return_maps ] ),
    sum_all_numbers_except_red( Json ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 6, solve1( "[1,2,3]" ) ),
      ?_assertEqual( 6, solve1( "{\"a\":2,\"b\":4}" ) ),
      ?_assertEqual( 3, solve1( "[[[3]]]" ) ),
      ?_assertEqual( 3, solve1( "{\"a\":{\"b\":4},\"c\":-1}" ) ),
      ?_assertEqual( 0, solve1( "{\"a\":[-1,1]}" ) ),
      ?_assertEqual( 0, solve1( "[-1,{\"a\":1}]" ) ),
      ?_assertEqual( 0, solve1( "[]" ) ),
      ?_assertEqual( 0, solve1( "{}" ) )
    ].

solve2_test_() ->
    [ ?_assertEqual( 6, solve2( "[1,2,3]" ) ),
      ?_assertEqual( 4, solve2( "[1,{\"c\":\"red\",\"b\":2},3]" ) ),
      ?_assertEqual( 0, solve2( "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" ) ),
      ?_assertEqual( 6, solve2( "[1,\"red\",5]" ) ) ].
