-module(problem2015_12).
-export([solve1/1, solve2/1]).

%%% PART 1

solve1( Input ) ->
    Separators = lists:seq( $a, $z ) ++ "{}[],\":",
    NumbersStrs = string:tokens( Input, Separators ),
    Numbers = lists:map( fun erlang:list_to_integer/1, NumbersStrs ),
    lists:sum( Numbers ).

%%% PART 2

solve2( Input ) ->
    Json = jiffy:decode( erlang:list_to_binary( Input ), [ return_maps ] ),
%    solve1( Input ) - count_red( Json ).

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
