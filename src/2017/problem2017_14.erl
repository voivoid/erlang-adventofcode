-module(problem2017_14).
-export([solve1/1, solve2/1, calc_hash/1]).

-type hash() :: nonempty_string().

-spec gen_inputs( string() ) -> list( string() ).
gen_inputs( Input ) ->
    lists:map( fun( N ) -> lists:concat( [ Input, "-", N ] ) end, lists:seq( 0, 127 ) ).

-spec calc_hash( string() ) -> hash().
calc_hash( Str ) ->
    problem2017_10:solve2( Str ).

-spec translate_to_bits1_sum( hash() ) -> non_neg_integer().
translate_to_bits1_sum( Hash ) ->
    Bin = << << ( erlang:list_to_integer( [ HexChar ], 16 ) ):8 >> || HexChar <- Hash >>,
    lists:sum( [ Bit || <<Bit:1>> <= Bin ] ).


-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Inputs = gen_inputs( Input ),
    Hashes = rpc:pmap({?MODULE, calc_hash}, [], Inputs ),
    Bits1List = lists:map( fun translate_to_bits1_sum/1, Hashes ),
    lists:sum( Bits1List ).

-spec solve2( string() ) -> non_neg_integer().
solve2( _Input ) ->
    0.



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 8108, solve1( "flqrgnkx" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 1242, solve2( "flqrgnkx" ) ) ].
