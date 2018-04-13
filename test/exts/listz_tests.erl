-module(listz_tests).
-include_lib("eunit/include/eunit.hrl").

index_test_() ->
    [ ?_assertEqual( 1, listz:index( a, [ a, b, c, d, e ] ) ),
      ?_assertEqual( 3, listz:index( c, [ a, b, c, d, e ] ) ),
      ?_assertEqual( not_found, listz:index( x, [ a, b, c, d, e ] ) ) ].

shiftl_test_() ->
    [ ?_assertEqual( [ 1, 2, 3 ], listz:shiftl( 0, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 2, 3, 1 ], listz:shiftl( 1, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 3, 1, 2 ], listz:shiftl( 2, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 1, 2, 3 ], listz:shiftl( 3, [ 1, 2, 3 ] ) ) ].

shiftr_test_() ->
    [ ?_assertEqual( [ 1, 2, 3 ], listz:shiftr( 0, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 3, 1, 2 ], listz:shiftr( 1, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 2, 3, 1 ], listz:shiftr( 2, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 1, 2, 3 ], listz:shiftr( 3, [ 1, 2, 3 ] ) ) ].

find_test_() ->
    [ ?_assertEqual( 2, listz:find( fun( X ) -> X == 2 end, [ 1, 2, 3 ] ) ),
      ?_assertEqual( not_found, listz:find( fun( X ) -> X > 5 end, [ 1, 2, 3 ] ) ) ].
