-module(zipper_tests).
-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                              fun( Z ) -> ?assertEqual( 1, zipper:get( Z ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 1 ], zipper:get_n( 1, Z ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 1, 2 ], zipper:get_n( 2, Z ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 1, 2, 3 ], zipper:get_n( 3, Z ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 1, 2, 3, 1 ], zipper:get_n( 4, Z ) ) end
    ] }.

next_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                              fun( Z ) -> ?assertEqual( { [ 1 ], [ 2, 3 ] }, zipper:next( Z ) ) end,
                                              fun( Z ) -> ?assertEqual( 1, zipper:get( zipper:next_n( 0, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 2, zipper:get( zipper:next_n( 1, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 3, zipper:get( zipper:next_n( 2, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 1, zipper:get( zipper:next_n( 3, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 3, zipper:get( zipper:next_n( -1, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 2, zipper:get( zipper:next_n( -2, Z ) ) ) end
                                             ] }.

prev_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                              fun( Z ) -> ?assertEqual( { [ 2, 1 ], [ 3 ] }, zipper:prev( Z ) ) end,
                                              fun( Z ) -> ?assertEqual( 1, zipper:get( zipper:prev_n( 0, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 3, zipper:get( zipper:prev_n( 1, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 2, zipper:get( zipper:prev_n( 2, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 1, zipper:get( zipper:prev_n( 3, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 2, zipper:get( zipper:prev_n( -1, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( 3, zipper:get( zipper:prev_n( -2, Z ) ) ) end
                                             ] }.

to_list_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                              fun( Z ) -> ?assertEqual( [ 1, 2, 3 ], zipper:to_list( Z ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 1, 2, 3 ], zipper:to_list( zipper:next_n( 1, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 1, 2, 3 ], zipper:to_list( zipper:next_n( 2, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 1, 2, 3 ], zipper:to_list( zipper:next_n( 3, Z ) ) ) end
                                             ] }.

update_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                              fun( Z ) -> ?assertEqual( 42, zipper:get( zipper:update( 42, Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 10, 11, 3 ], zipper:to_list( zipper:update_with_list( [ 10, 11 ], Z ) ) ) end,
                                              fun( Z ) -> ?assertEqual( [ 13, 11, 12 ], zipper:to_list( zipper:update_with_list( [ 10, 11, 12, 13 ], Z ) ) ) end
                                             ] }.

prepend_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                           fun( Z ) -> ?assertEqual( [ 0, 1, 2, 3 ], zipper:to_list( zipper:prepend( 0, zipper:next_n( 0, Z ) ) ) ) end,
                                           fun( Z ) -> ?assertEqual( [ 1, 0, 2, 3 ], zipper:to_list( zipper:prepend( 0, zipper:next_n( 1, Z ) ) ) ) end,
                                           fun( Z ) -> ?assertEqual( [ 1, 2, 0, 3 ], zipper:to_list( zipper:prepend( 0, zipper:next_n( 2, Z ) ) ) ) end,
                                           fun( Z ) -> ?assertEqual( [ 0, 1, 2, 3 ], zipper:to_list( zipper:prepend( 0, zipper:next_n( 3, Z ) ) ) ) end
] }.

append_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                           fun( Z ) -> ?assertEqual( [ 1, 0, 2, 3 ], zipper:to_list( zipper:append( 0, zipper:next_n( 0, Z ) ) ) ) end,
                                           fun( Z ) -> ?assertEqual( [ 1, 2, 0, 3 ], zipper:to_list( zipper:append( 0, zipper:next_n( 1, Z ) ) ) ) end,
                                           fun( Z ) -> ?assertEqual( [ 1, 2, 3, 0 ], zipper:to_list( zipper:append( 0, zipper:next_n( 2, Z ) ) ) ) end,
                                           fun( Z ) -> ?assertEqual( [ 1, 0, 2, 3 ], zipper:to_list( zipper:append( 0, zipper:next_n( 3, Z ) ) ) ) end
] }.

is_first_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                           fun( Z ) -> ?assert( zipper:is_first( zipper:next_n( 0, Z ) ) ) end,
                                           fun( Z ) -> ?assertNot( zipper:is_first( zipper:next_n( 1, Z ) ) ) end,
                                           fun( Z ) -> ?assertNot( zipper:is_first( zipper:next_n( 2, Z ) ) ) end,
                                           fun( Z ) -> ?assert( zipper:is_first( zipper:next_n( 3, Z ) ) ) end
] }.

is_last_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                           fun( Z ) -> ?assertNot( zipper:is_last( zipper:next_n( 0, Z ) ) ) end,
                                           fun( Z ) -> ?assertNot( zipper:is_last( zipper:next_n( 1, Z ) ) ) end,
                                           fun( Z ) -> ?assert( zipper:is_last( zipper:next_n( 2, Z ) ) ) end,
                                           fun( Z ) -> ?assertNot( zipper:is_last( zipper:next_n( 3, Z ) ) ) end
] }.

pos_test_() ->
    { with, zipper:from_list( [ 1, 2, 3 ] ), [
                                           fun( Z ) -> ?assertEqual( 1, zipper:pos( zipper:next_n( 0, Z ) ) ) end,
                                           fun( Z ) -> ?assertEqual( 2, zipper:pos( zipper:next_n( 1, Z ) ) ) end,
                                           fun( Z ) -> ?assertEqual( 3, zipper:pos( zipper:next_n( 2, Z ) ) ) end,
                                           fun( Z ) -> ?assertEqual( 1, zipper:pos( zipper:next_n( 3, Z ) ) ) end
                                          ] }.


is_empty_test_() ->
    [
     ?_assert( zipper:is_empty( zipper:from_list( [] ) ) ),
     ?_assertNot( zipper:is_empty( zipper:from_list( [ 1 ] ) ) )
    ].

len_test_() ->
    [
     ?_assertEqual( 0, zipper:len( zipper:from_list( [] ) ) ),
     ?_assertEqual( 3, zipper:len( zipper:from_list( [ 1, 2, 3 ] ) ) )
    ].
