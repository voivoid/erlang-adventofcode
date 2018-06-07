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

iterate_test_() ->
    [ ?_assertEqual( [], listz:iterate( fun( X ) -> X end, '_', 0 ) ),
      ?_assertEqual( [ x ], listz:iterate( fun( X ) -> X end, x, 1 ) ),
      ?_assertEqual( [ 1, 2, 3, 4, 5 ], listz:iterate( fun( X ) -> X + 1 end, 1, 5 ) ) ].

foldl_stoppable_test_() ->
    [ ?_assertEqual( 42, listz:foldl_stoppable( fun( _, Acc  ) -> Acc end, 42, '_', [] ) ),
      ?_assertEqual( 55, listz:foldl_stoppable( fun erlang:'+'/2, 0, '_', lists:seq( 1, 10 ) ) ),
      ?_assertEqual( 15, listz:foldl_stoppable( fun( _, Acc ) when Acc > 10 -> { stop, Acc };
                                                   ( X, Acc ) -> X + Acc
                                                end,
                                                0,
                                                stop,
                                                lists:seq( 1, 10 ) ) ) ].
permutations_test_() ->
    [ ?_assertEqual( [ [] ],    listz:permutations( [] ) ),
      ?_assertEqual( [ [ 1 ] ], listz:permutations( [ 1 ] ) ),
      ?_assertEqual( [ [ 1, 2 ], [ 2, 1 ] ], listz:permutations( [ 1, 2 ] ) ),
      ?_assertEqual( [ [ 1, 2, 3 ], [ 1, 3, 2 ], [ 2, 1, 3 ], [ 2, 3, 1 ], [ 3, 1, 2 ], [ 3, 2, 1 ] ], listz:permutations( [ 1, 2, 3 ] ) )
    ].

set_elem_test_() ->
    [ ?_assertEqual( [ y ], listz:set_elem( y, 1, [ x ] ) ),
      ?_assertEqual( [ x, a, z ], listz:set_elem( a, 2, [ x, y, z ] ) ),
      ?_assertEqual( [ x, y, a ], listz:set_elem( a, 3, [ x, y, z ] ) ),
      ?_assertError( badarg, listz:set_elem( e, 0, [] ) ),
      ?_assertError( badarg, listz:set_elem( e, 1, [] ) )
    ].

insert_test_() ->
    [ ?_assertEqual( [ x, y, z ], listz:insert( [], 0, [ x, y, z ] ) ),
      ?_assertEqual( [ x, y, z ], listz:insert( [], 3, [ x, y, z ] ) ),
      ?_assertEqual( [ a, x, y, z, b ], listz:insert( [ x, y, z ], 1, [ a, b ] ) ),
      ?_assertEqual( [ a, b, c, x, y, z ], listz:insert( [ x, y, z ], 3, [ a, b, c ] ) )
    ].

replace_test_() ->
    [ ?_assertEqual( [ y, z ], listz:replace( [], 1, [ x, y, z ] ) ),
      ?_assertEqual( [ x, y ], listz:replace( [], 3, [ x, y, z ] ) ),
      ?_assertEqual( [ x, y, z, b ], listz:replace( [ x, y, z ], 1, [ a, b ] ) ),
      ?_assertEqual( [ a, b, x, y, z ], listz:replace( [ x, y, z ], 3, [ a, b, c ] ) )
    ].
