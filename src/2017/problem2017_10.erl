-module(problem2017_10).
-export([solve1/1]).

process_len( Len, { Zipper, Skip } ) ->
    ReversedFirstNums = lists:reverse( zipper:get_n( Len, Zipper ) ),
    UpdatedFirstNums = zipper:update_with_list( ReversedFirstNums, Zipper ),


    { zipper:next_n( Skip, UpdatedFirstNums ), Skip + 1 }.

solve1( Input, N ) ->
    Lengths = lists:map( fun erlang:list_to_integer/1, string:tokens( Input, ", " ) ),
    Zipper = zipper:make( lists:seq( 0, N ) ),

    { UpdatedZipper, _ } = lists:foldl( fun process_len/2, { Zipper, 0 }, Lengths ),
    [ A, B | _ ] = zipper:to_list( UpdatedZipper ),
    A * B.

solve1( Input ) ->
    solve1( Input, 255 ).


-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 12, solve1( "3, 4, 1, 5", 4 ) ) ].
