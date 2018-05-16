-module(problem2016_05).
-export([solve1/1, solve2/1]).
-compile([export_all, nowarn_export_all]).

%%% COMMON

-type doorid() :: string().
-type doorid_postfix() :: non_neg_integer().
-type md5() :: string().
-type md5_prefix() :: md5().
-type password() :: string().
-type letters_map() :: #{ non_neg_integer() := char() }.

-spec make_md5( doorid(), doorid_postfix() ) -> md5().
make_md5( DoorID, Postfix ) ->
    algos:get_md5_str( DoorID ++ erlang:integer_to_list( Postfix ) ).

-spec find_prefixed_md5( doorid(), doorid_postfix(), md5_prefix() ) -> { doorid_postfix(), md5() }.
find_prefixed_md5( DoorID, PostfixNum, Prefix ) ->
    MD5 = make_md5( DoorID, PostfixNum ),
    case lists:prefix( Prefix, MD5 ) of
        true -> { PostfixNum, MD5 };
        false -> find_prefixed_md5( DoorID, PostfixNum + 1, Prefix )
    end.

%%% PART 1

-spec find_password1( doorid() ) -> password().
find_password1( DoorID ) ->
    { _, ReversedPassword } = lists:foldl( fun( _, { PostfixNum, PasswordAcc } ) ->
                                                   { MD5PostfixNum, MD5 } = find_prefixed_md5( DoorID, PostfixNum, "00000" ),
                                                   NextPasswordChar = lists:nth( 6, MD5 ),
                                                   { MD5PostfixNum + 1, [ NextPasswordChar | PasswordAcc ] }
                                           end,
                                           { 0, "" },
                                   lists:seq( 1, 8 ) ),
    lists:reverse( ReversedPassword ).


-spec solve1( string() ) -> string().
solve1( Input ) ->
    find_password1( Input ).

%%% PART 2

-spec is_password_found( letters_map() ) -> boolean().
is_password_found( LettersMap ) ->
    maps:size( LettersMap ) == 8.

-spec update_password( letters_map(), char(), non_neg_integer() ) -> letters_map().
update_password( LettersMap, Char, Pos ) ->
    case Pos >= 0 andalso Pos =< 7 andalso ( not maps:is_key( Pos, LettersMap ) ) of
        true -> LettersMap#{ Pos => Char };
        false -> LettersMap
    end.

-spec find_password2( doorid(), letters_map(), doorid_postfix() ) -> password().
find_password2( DoorID, LettersMap, PostfixNum ) ->
    case is_password_found( LettersMap ) of
        true -> { _, Password } = lists:unzip( lists:sort( maps:to_list( LettersMap ) ) ),
                Password;
        false -> { MD5PostfixNum, MD5 } = find_prefixed_md5( DoorID, PostfixNum, "00000" ),
                 Pos = erlang:list_to_integer( [ lists:nth( 6, MD5 ) ], 16 ),
                 Char = lists:nth( 7, MD5 ),
                 NextPassword = update_password( LettersMap, Char, Pos ),
                 find_password2( DoorID, NextPassword, MD5PostfixNum + 1 )
    end.

-spec solve2( string() ) -> string().
solve2( Input ) ->
    find_password2( Input, #{}, 0 ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    { timeout, 60, ?_assertEqual( "18F47A30", solve1( "abc" ) ) }.

solve2_test_() ->
     { timeout, 60, ?_assertEqual( "05ACE8E3", solve2( "abc" ) ) }.
