-module(problem2017_03).
-export([solve1/1]).

solve1( _Input ) ->
    0.


-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 0, solve1( "1" ) ),
      ?_assertEqual( 3, solve1( "12" ) ),
      ?_assertEqual( 2, solve1( "23" ) ),
      ?_assertEqual( 31, solve1( "1024" ) ) ].
