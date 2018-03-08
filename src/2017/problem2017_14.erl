-module(problem2017_14).
-export([solve1/1, solve2/1]).



-type hash() :: nonempty_string().
-type bit() :: $0 | $1.
-type bits() :: list( bit() ).

-spec gen_inputs( string() ) -> list( string() ).
gen_inputs( Input ) ->
    lists:map( fun( N ) -> lists:concat( [ Input, "-", N ] ) end, lists:seq( 0, 127 ) ).

-spec calc_hash( string() ) -> hash().
calc_hash( Str ) ->
    problem2017_10:solve2( Str ).

-spec translate_to_bits( hash() ) -> bits().
translate_to_bits( Hash ) ->
    lists:concat( lists:map( fun( HexChr ) -> erlang:integer_to_list( erlang:list_to_integer( [ HexChr ], 16 ), 2 ) end, Hash ) ).

-spec count_bits( bits() ) -> list( non_neg_integer() ).
count_bits( BitsList ) ->
    lists:map( fun( Bits ) -> 
                       lists:foldl( fun ( $1, Acc ) -> Acc + 1; 
                                        ( $0, Acc ) -> Acc
                                    end,
                                    0,
                                    Bits )
               end, 
               BitsList ).


-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Inputs = gen_inputs( Input ),
    Hashes =  lists:map( fun calc_hash/1, Inputs ),
    BitsList = lists:map( fun translate_to_bits/1, Hashes ),
    lists:sum( count_bits( BitsList ) ).

-spec solve2( string() ) -> non_neg_integer().
solve2( _Input ) ->
    0.



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ { timeout, 30, ?_assertEqual( 8108, solve1( "flqrgnkx" ) ) } ].

solve2_test_() ->
    [ ?_assertEqual( 1242, solve2( "flqrgnkx" ) ) ].
