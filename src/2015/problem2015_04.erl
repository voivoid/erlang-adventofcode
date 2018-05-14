-module(problem2015_04).
-export([solve1/1, solve2/1]).

%%% COMMON

-spec get_md5_str( string() ) -> string().
get_md5_str( Str ) ->
    MD5 = erlang:md5( Str ),
    lists:append( [ integer_to_list( X, 16 ) || <<X:4>> <= MD5 ] ).

-spec make_md5( string(), non_neg_integer() ) -> string().
make_md5( SecretKey, PostfixNum ) ->
    get_md5_str( SecretKey ++ erlang:integer_to_list( PostfixNum ) ).

-spec find_prefixed_md5( string(), non_neg_integer(), string() ) -> non_neg_integer().
find_prefixed_md5( SecretKey, PostfixNum, Prefix ) ->
    MD5 = make_md5( SecretKey, PostfixNum ),
    case lists:prefix( Prefix, MD5 ) of
        true -> PostfixNum;
        false -> find_prefixed_md5( SecretKey, PostfixNum + 1, Prefix )
    end.

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    find_prefixed_md5( Input, 0, "00000" ).

%%% PART 2

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    find_prefixed_md5( Input, 0, "000000" ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 609043, solve1( "abcdef" ) ),
      ?_assertEqual( 1048970, solve1( "pqrstuv" ) ) ].
