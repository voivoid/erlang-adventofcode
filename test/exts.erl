-module( exts ).
-include_lib("eunit/include/eunit.hrl").

algos_iterate_test_() ->
    [ ?_assertEqual( 5, algos:iterate( fun (X) -> X + 1 end, 1, 4 ) ) ].

lists_exts_index_test_() ->
    [ ?_assertEqual( 1, lists_exts:index( a, [ a, b, c ] ) ),
      ?_assertEqual( 3, lists_exts:index( 3, [ 1, 2, 3, 4, 5 ] ) ),
      ?_assertEqual( not_found, lists_exts:index( x, [ 1, 2, 3 ] ) ) ].

lists_exts_shiftl_test_() ->
    [ ?_assertEqual( [ 1, 2, 3 ], lists_exts:shiftl( 0, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 2, 3, 1 ], lists_exts:shiftl( 1, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 3, 1, 2 ], lists_exts:shiftl( 2, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 1, 2, 3 ], lists_exts:shiftl( 3, [ 1, 2, 3 ] ) ) ].

lists_exts_shiftr_test_() ->
    [ ?_assertEqual( [ 1, 2, 3 ], lists_exts:shiftr( 0, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 3, 1, 2 ], lists_exts:shiftr( 1, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 2, 3, 1 ], lists_exts:shiftr( 2, [ 1, 2, 3 ] ) ),
      ?_assertEqual( [ 1, 2, 3 ], lists_exts:shiftr( 3, [ 1, 2, 3 ] ) ) ].


zipper_test_() ->
    [ { with, zipper:make( [ 1, 2, 3 ] ), [
                                           fun( Z ) -> ?assertEqual( { [], [ 1, 2, 3 ] }, Z ) end,

                                           fun( Z ) -> ?assertEqual( { [ 1 ], [ 2, 3 ] }, zipper:next( Z ) ) end,
                                           fun( Z ) -> ?assertEqual( { [ 2, 1 ], [ 3 ] }, zipper:next_n( Z, 2 ) ) end,
                                           fun( Z ) -> ?assertEqual( { [ 3, 2, 1 ], [] }, zipper:next_n( Z, 3 ) ) end,
                                           fun( Z ) -> ?assertEqual( { [], [ 1, 2, 3 ] }, zipper:next_n( Z, 4 ) ) end,

                                           fun( Z ) -> ?assertEqual( 1, zipper:get( zipper:next_n( Z, 0 ) ) ) end,
                                           fun( Z ) -> ?assertEqual( 2, zipper:get( zipper:next_n( Z, 1 ) ) ) end,
                                           fun( Z ) -> ?assertEqual( 3, zipper:get( zipper:next_n( Z, 2 ) ) ) end,
                                           fun( Z ) -> ?assertError( function_clause, zipper:get( zipper:next_n( Z, 3 ) ) ) end,

                                           fun( Z ) -> ?assertEqual( 42, zipper:get( zipper:update( 42, Z ) ) ) end,
                                           fun( Z ) -> ?assertEqual( [ 1, 2, 3 ], zipper:to_list( Z ) ) end,
                                           fun( Z ) -> ?assertEqual( [ 1, 2, 3 ], zipper:to_list( zipper:next_n( Z, 2 ) ) ) end
                                          ]
      }
    ].
