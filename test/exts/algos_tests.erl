-module(algos_tests).
-include_lib("eunit/include/eunit.hrl").

iterate_test_() ->
    [ ?_assertEqual( 5, algos:iterate( fun (X) -> X + 1 end, 1, 4 ) ) ].

md5_test_() ->
    [ ?_assertEqual( "698D51A19D8A121CE581499D7B701668", algos:get_md5_str( "111" ) ),
      ?_assertEqual( "E8DC4081B13434B45189A720B77B6818", algos:get_md5_str( "abcdefgh" ) ) ].
