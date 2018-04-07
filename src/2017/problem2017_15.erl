-module(problem2017_15).
-export([solve1/1]).

gen_next_value( Val, Factor ) ->
    ( Val * Factor ) rem 2147483647.

gen_next_a_value( Val ) ->
    gen_next_value( Val, 16807 ).

gen_next_b_value( Val ) ->
    gen_next_value( Val, 48271 ).

match_lower_16_bits( A, B ) ->
    ( A band 16#FFFF ) == ( B band 16#FFFF ).
    
count_matches( _, _, 0, Matches ) -> Matches;
count_matches( A, B, PairsNum, Matches ) ->
    A2 = gen_next_a_value( A ),
    B2 = gen_next_b_value( B ),
    NewMatches = case match_lower_16_bits( A2, B2 ) of
                     true  -> Matches + 1;
                     false -> Matches
                 end,
    count_matches( A2, B2, PairsNum - 1, NewMatches ).

count_matches( A, B, PairsNum ) ->
    count_matches( A, B, PairsNum, 0 ).

get_gen_input_value( Line ) ->
    [ _, _, _, _, Val ] = string:tokens( Line, " " ),
    erlang:list_to_integer( Val ).

solve1( Input ) ->
    [ Line1, Line2 ] = string:tokens( Input, "\n" ),
    A = get_gen_input_value( Line1 ),
    B = get_gen_input_value( Line2 ),
    count_matches( A, B, 40000000 ).

-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 588, solve1("Generator A starts with 65\nGenerator B starts with 8921") ) ].
