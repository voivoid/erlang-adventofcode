-module(problem2017_13).
-export([solve1/1]).

solve1( _Input ) ->
    0.


-include_lib("eunit/include/eunit.hrl").


test_input() ->
    "0: 3
     1: 2
     4: 4
     6: 4".

solve1_test_() ->
    [ ?_assertEqual( 24, solve1( test_input() ) ) ].
