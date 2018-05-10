-module(problem2015_11).
-export([solve1/1, solve2/1]).

-type password() :: string().

%%% COMMON

-spec next_letter( char() ) -> { char(), boolean() }.
next_letter( $z ) -> { $a, true };
next_letter( L ) -> { L + 1, false }.

-spec has_incrementing_letters( password() ) -> boolean().
has_incrementing_letters( [ C, B, A | _ ] ) when B == A + 1, C == B + 1 ->
    true;
has_incrementing_letters( [ _ | Rest ] ) -> has_incrementing_letters( Rest );
has_incrementing_letters( [] ) -> false.

-spec has_no_forbidden_letters( password() ) -> boolean().
has_no_forbidden_letters( [] ) -> true;
has_no_forbidden_letters( [ X | XS ] ) ->
    case lists:member( X, "iol" ) of
        true -> false;
        false -> has_no_forbidden_letters( XS )
    end.

-spec has_two_letter_pairs( password() ) -> boolean().
has_two_letter_pairs( Password ) ->
    has_two_letter_pairs_impl( Password, 0 ).

-spec has_two_letter_pairs_impl( password(), 0 | 1 ) -> boolean().
has_two_letter_pairs_impl( [], _ ) ->
    false;
has_two_letter_pairs_impl( [ A, A | XS ], Counter ) ->
    case Counter of
        0 -> has_two_letter_pairs_impl( XS, 1 );
        1 -> true
    end;
has_two_letter_pairs_impl( [ _ | XS ], Counter ) ->
    has_two_letter_pairs_impl( XS, Counter ).

-spec is_good_password( password() ) -> boolean().
is_good_password( Password ) ->
    has_incrementing_letters( Password )
        andalso has_no_forbidden_letters( Password )
        andalso has_two_letter_pairs( Password ).

-spec get_next_password( password() ) -> password().
get_next_password( [] ) -> "a";
get_next_password( [ X | XS ] ) ->
    case next_letter( X ) of
        { NextLetter, false } -> [ NextLetter | XS ];
        { NextLetter, true } -> [ NextLetter | get_next_password( XS ) ]
    end.

find_good_password( Password ) ->
    NextPass = get_next_password( Password ),
    case is_good_password( NextPass ) of
        true -> NextPass;
        false -> find_good_password( NextPass )
    end.

%%% PART 1

solve1( Input ) ->
    lists:reverse( find_good_password( lists:reverse( Input ) ) ).

%%% PART 2

solve2( Input ) ->
    solve1( solve1( Input ) ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [
     ?_assert( has_incrementing_letters( "xxxcbayyy" ) ),
     ?_assert( has_incrementing_letters( "cba" ) ),
     ?_assertNot( has_incrementing_letters( "zzzyyyxxx" ) ),

     ?_assertNot( has_no_forbidden_letters( "o" ) ),
     ?_assert( has_no_forbidden_letters( "a" ) ),

     ?_assert( has_two_letter_pairs( "abcddefgghi" ) ),
     ?_assertNot( has_two_letter_pairs( "abcddefg" ) ),

     ?_assertEqual( "b", get_next_password( "a" ) ),
     ?_assertEqual( "aa", get_next_password( "z" ) ),
     ?_assertEqual( "ab", get_next_password( "za" ) ),
     ?_assertEqual( "bbc", get_next_password( "abc" ) ),
     ?_assertEqual( "aae", get_next_password( "zzd" ) ),

     ?_assertNot( is_good_password( "hijklmmn" ) ),
     ?_assertNot( is_good_password( "abbceffg" ) ),
     ?_assertNot( is_good_password( "abbcegjk" ) ),

     ?_assertEqual( "abcdffaa", solve1( "abcdefgh" ) ),
     ?_assertEqual( "ghjaabcc", solve1( "ghijklmn" ) )
    ].
