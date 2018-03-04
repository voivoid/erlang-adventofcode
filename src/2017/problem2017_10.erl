-module(problem2017_10).
-export([solve1/1, solve2/1]).

-type len() :: non_neg_integer().
-type num() :: non_neg_integer().
-type skip() :: non_neg_integer().
-type process_state() :: { zipper:zipper( num() ), skip() }.

-spec process_len( len(), process_state() ) -> process_state().
process_len( Len, { NumsZipper, Skip } ) ->
    ReversedFirstNums = lists:reverse( zipper:get_n( Len, NumsZipper ) ),
    UpdatedNumsZipper = zipper:update_with_list( ReversedFirstNums, NumsZipper ),

    { zipper:next_n( Skip, UpdatedNumsZipper ), Skip + 1 }.

-spec run_processing( list( len() ), process_state() ) -> process_state().
run_processing( Lengths, State ) ->
    lists:foldl( fun process_len/2, State, Lengths ).

-spec reduce_to_dense_hash( list( num() ) ) -> list( num() ).
reduce_to_dense_hash( [] ) -> [];
reduce_to_dense_hash( Nums ) ->
    { First16, Rest } = lists:split( 16, Nums ),
    XoredNum = lists:foldl( fun erlang:'bxor'/2, 0, First16 ),
    [ XoredNum | reduce_to_dense_hash( Rest ) ].

-spec num_to_hex_str( num() ) -> string().
num_to_hex_str( Dec ) ->
    case erlang:integer_to_list( Dec, 16) of
        [C] -> [ $0, C ];
        [_, _] = S -> S
    end.

-spec hash_nums_to_hex_str( list( num() ) ) -> string().
hash_nums_to_hex_str( HashNums ) ->
    HashStr = lists:foldl( fun( Num, Acc ) -> num_to_hex_str( Num ) ++ Acc end,
                           [],
                           lists:reverse( HashNums ) ),
    string:to_lower( HashStr ).

-spec solve1( string(), non_neg_integer() ) -> non_neg_integer().
solve1( Input, N ) ->
    Lengths = lists:map( fun erlang:list_to_integer/1, string:tokens( Input, ", " ) ),
    InitNumsZipper = zipper:make( lists:seq( 0, N ) ),
    { ResultNumsZipper, _ } = run_processing( Lengths, { InitNumsZipper, 0 } ),

    [ A, B | _ ] = zipper:to_list( ResultNumsZipper ),
    A * B.

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve1( Input, 255 ).

-spec solve2( string() ) -> string().
solve2( Input ) ->
    Lengths = Input ++ [ 17, 31, 73, 47, 23 ],
    InitNumsZipper = zipper:make( lists:seq( 0, 255 ) ),
    { ResultNumsZipper, _ } = lists:foldl( fun (_, State) -> run_processing( Lengths, State ) end,
                                       { InitNumsZipper, 0 },
                                       lists:seq( 1, 64 ) ),
    HashNums = reduce_to_dense_hash( zipper:to_list( ResultNumsZipper ) ),

    hash_nums_to_hex_str( HashNums ).


-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 12, solve1( "3, 4, 1, 5", 4 ) ) ].

solve2_test_() ->
    [ ?_assertEqual( "a2582a3a0e66e6e86e3812dcb672a272", solve2( "" ) ),
      ?_assertEqual( "33efeb34ea91902bb2f59c9920caa6cd", solve2( "AoC 2017" ) ), 
      ?_assertEqual( "3efbe78a8d82f29979031a4aa0b16a9d", solve2( "1,2,3" ) ),
      ?_assertEqual( "63960835bcdc130f0b66d7ff4f6a5a8e", solve2( "1,2,4" ) ) ].
