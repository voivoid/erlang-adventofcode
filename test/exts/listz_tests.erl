-module(listz_tests).
-include_lib("eunit/include/eunit.hrl").

index_test_() ->
    { with, [ a, b, c, d, e ],
    [ fun( L ) -> ?_assertEqual( 1, listz:index( a, L ) ) end,
      fun( L ) -> ?_assertEqual( 3, listz:index( c, L ) ) end,
      fun( L ) -> ?_assertEqual( not_found, listz:index( x, L ) ) end ] }.

shiftl_test_() ->
    { with, [ 1, 2, 3 ],
    [ fun( L ) -> ?_assertEqual( [ 1, 2, 3 ], listz:shiftl( 0, L ) ) end,
      fun( L ) -> ?_assertEqual( [ 2, 3, 1 ], listz:shiftl( 1, L ) ) end,
      fun( L ) -> ?_assertEqual( [ 3, 1, 2 ], listz:shiftl( 2, L ) ) end,
      fun( L ) -> ?_assertEqual( [ 1, 2, 3 ], listz:shiftl( 3, L ) ) end ] }.

shiftr_test_() ->
    { with, [ 1, 2, 3 ],
    [ fun( L ) -> ?_assertEqual( [ 1, 2, 3 ], listz:shiftr( 0, L ) ) end,
      fun( L ) -> ?_assertEqual( [ 3, 1, 2 ], listz:shiftr( 1, L ) ) end,
      fun( L ) -> ?_assertEqual( [ 2, 3, 1 ], listz:shiftr( 2, L ) ) end,
      fun( L ) -> ?_assertEqual( [ 1, 2, 3 ], listz:shiftr( 3, L ) ) end ] }.

