-module(problem2017_15).
-export([solve1/1, solve2/1]).

-define(REM, 2147483647).

-spec gen_next_1a_value( non_neg_integer() ) -> non_neg_integer().
gen_next_1a_value( Val ) ->
    ( Val * 16807 ) rem ?REM.

-spec gen_next_1b_value( non_neg_integer() ) -> non_neg_integer().
gen_next_1b_value( Val ) ->
    ( Val * 48271 ) rem ?REM.

-spec gen_next_2a_value( non_neg_integer() ) -> non_neg_integer().
gen_next_2a_value( Val ) ->
    case gen_next_1a_value( Val ) of
        Next when Next rem 4 == 0 -> Next;
        Next -> gen_next_2a_value( Next )
    end.

-spec gen_next_2b_value( non_neg_integer() ) -> non_neg_integer().
gen_next_2b_value( Val ) ->
    case gen_next_1b_value( Val ) of
        Next when Next rem 8 == 0 -> Next;
        Next -> gen_next_2b_value( Next )
    end.

-spec match_lower_16_bits( non_neg_integer(), non_neg_integer() ) -> 0 | 1.
match_lower_16_bits( A, B ) ->
    FirstBits = 16#FFFF,
    case ( A band FirstBits ) == ( B band FirstBits ) of
        true -> 1;
        false -> 0
    end.
    
-spec count_matches_1( non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer() ) -> non_neg_integer().
count_matches_1( _, _, 0, Matches ) -> Matches;
count_matches_1( A, B, PairsNum, Matches ) ->
    A2 = gen_next_1a_value( A ),
    B2 = gen_next_1b_value( B ),
    count_matches_1( A2, B2, PairsNum - 1, Matches + match_lower_16_bits( A2, B2 ) ).

-spec count_matches_2( non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer() ) -> non_neg_integer().
count_matches_2( _, _, 0, Matches ) -> Matches;
count_matches_2( A, B, PairsNum, Matches ) ->
    A2 = gen_next_2a_value( A ),
    B2 = gen_next_2b_value( B ),
    count_matches_2( A2, B2, PairsNum - 1, Matches + match_lower_16_bits( A2, B2 ) ).

-spec get_gen_input_value( nonempty_string() ) -> non_neg_integer().
get_gen_input_value( Line ) ->
    [ _, _, _, _, Val ] = string:tokens( Line, " " ),
    erlang:list_to_integer( Val ).

-spec get_gen_inputs( nonempty_string() ) -> { non_neg_integer(), non_neg_integer() }.
get_gen_inputs( Input ) ->
    [ Line1, Line2 ] = string:tokens( Input, "\n" ),
    A = get_gen_input_value( Line1 ),
    B = get_gen_input_value( Line2 ),
    { A, B }.

-spec solve1( nonempty_string() ) -> non_neg_integer().
solve1( Input ) ->
    { A, B } = get_gen_inputs( Input ),
    count_matches_1( A, B, 40000000, 0 ).

-spec solve2( nonempty_string() ) -> non_neg_integer().
solve2( Input ) ->
    { A, B } = get_gen_inputs( Input ),
    count_matches_2( A, B, 5000000, 0 ).



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 588, solve1("Generator A starts with 65\nGenerator B starts with 8921") ) ].

solve2_test_() ->
    [ ?_assertEqual( 309, solve2("Generator A starts with 65\nGenerator B starts with 8921") ) ].
