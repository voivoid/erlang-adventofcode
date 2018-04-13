-module(algos_tests).
-include_lib("eunit/include/eunit.hrl").

iterate_test_() ->
    [ ?_assertEqual( 5, algos:iterate( fun (X) -> X + 1 end, 1, 4 ) ) ].
