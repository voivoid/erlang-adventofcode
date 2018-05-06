-module(problem2015_05).
-export([solve1/1, solve2/1]).

%%% COMMON

-spec solve( string(), fun( ( string() ) -> boolean() ) ) -> non_neg_integer().
solve( Input, LineChecker ) ->
    Lines = string:tokens( Input, "\n" ),
    GoodLines = lists:filter( LineChecker, Lines ),
    erlang:length( GoodLines ).

%%% PART 1

-spec is_vowel( char() ) -> boolean().
is_vowel( C ) ->
    lists:member( C, "aeiou" ).

-spec contains_3_vowels( string() ) -> boolean().
contains_3_vowels( Input ) ->
    erlang:length( lists:filter( fun is_vowel/1, Input ) ) >= 3.

-spec has_double_letter( string() ) -> boolean().
has_double_letter( [] ) -> false;
has_double_letter( [ X, X | _ ] ) -> true;
has_double_letter( [ _ | XS ] ) -> has_double_letter( XS ).

-spec has_no_forbidden_strings( string() ) -> boolean().
has_no_forbidden_strings( [] ) -> true;
has_no_forbidden_strings( [ $a, $b | _ ] ) -> false;
has_no_forbidden_strings( [ $c, $d | _ ] ) -> false;
has_no_forbidden_strings( [ $p, $q | _ ] ) -> false;
has_no_forbidden_strings( [ $x, $y | _ ] ) -> false;
has_no_forbidden_strings( [ _ | XS ] ) -> has_no_forbidden_strings( XS ).

-spec is_good_line1( string() ) -> boolean().
is_good_line1( Input ) ->
    contains_3_vowels( Input )
        andalso has_double_letter( Input )
        andalso has_no_forbidden_strings( Input ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve( Input, fun is_good_line1/1 ).

%%% PART 2

-spec has_pair_of_letters( string() ) -> boolean().
has_pair_of_letters( [] ) -> false;
has_pair_of_letters( [ X, Y | XS ] ) -> case string:str( XS, [ X, Y ] ) of
                                            0 -> has_pair_of_letters( [ Y | XS ] );
                                            _ -> true
                                        end;
has_pair_of_letters( [ _ | XS ] ) -> has_pair_of_letters( XS ).

-spec has_repeating_letter( string() ) -> boolean().
has_repeating_letter( [] ) -> false;
has_repeating_letter( [ X, _, X | _ ] ) -> true;
has_repeating_letter( [ _ | XS ] ) -> has_repeating_letter( XS ).

-spec is_good_line2( string() ) -> boolean().
is_good_line2( Input ) ->
    has_pair_of_letters( Input ) andalso has_repeating_letter( Input ).

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    solve( Input, fun is_good_line2/1 ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 1, solve1( "ugknbfddgicrmopn" ) ),
      ?_assertEqual( 1, solve1( "aaa" ) ),
      ?_assertEqual( 0, solve1( "jchzalrnumimnmhp" ) ),
      ?_assertEqual( 0, solve1( "haegwjzuvuyypxyu" ) ),
      ?_assertEqual( 0, solve1( "dvszwmarrgswjxmb" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 1, solve2( "qjhvhtzxzqqjkmpb" ) ),
      ?_assertEqual( 1, solve2( "xxyxx" ) ),
      ?_assertEqual( 0, solve2( "uurcxstgmygtbstg" ) ),
      ?_assertEqual( 0, solve2( "ieodomkazucvgmuy" ) ) ].
