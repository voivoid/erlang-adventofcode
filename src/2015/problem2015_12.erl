-module(problem2015_12).
-export([solve1/1]).

solve1( Input ) ->
    Separators = lists:seq( $a, $z ) ++ "{}[],\":",
    NumbersStrs = string:tokens( Input, Separators ),
    Numbers = lists:map( fun erlang:list_to_integer/1, NumbersStrs ),
    lists:sum( Numbers ).

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
