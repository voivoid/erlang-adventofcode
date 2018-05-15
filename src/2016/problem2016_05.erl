-module(problem2016_05).
-export([solve1/1]).
-compile([export_all, nowarn_export_all]).

%%% COMMON

-spec make_md5( string(), non_neg_integer() ) -> string().
make_md5( DoorID, PostfixNum ) ->
    algos:get_md5_str( DoorID ++ erlang:integer_to_list( PostfixNum ) ).

-spec find_prefixed_md5( string(), non_neg_integer(), string() ) -> non_neg_integer().
find_prefixed_md5( DoorID, PostfixNum, Prefix ) ->
    MD5 = make_md5( DoorID, PostfixNum ),
    case lists:prefix( Prefix, MD5 ) of
        true -> { PostfixNum, lists:nth( 6, MD5 ) };
        false -> find_prefixed_md5( DoorID, PostfixNum + 1, Prefix )
    end.

find_password( DoorID ) ->
    { _, PasswordAcc } = lists:foldl( fun( _, { PostfixNum, PasswordAcc } ) ->
                                              { NextPostfixNum, NextPasswordChar } = find_prefixed_md5( DoorID, PostfixNum, "00000" ),
                                              { NextPostfixNum + 1, [ NextPasswordChar | PasswordAcc ] }
                                      end,
                                      { 0, "" },
                                      lists:seq( 1, 8 ) ),
    lists:reverse( PasswordAcc ).

%%% PART 1

solve1( Input ) ->
    find_password( Input ).

%%% PART 2

solve2( Input ) ->
    0.

%%% TESTS

%% -include_lib("eunit/include/eunit.hrl").

%% solve1_test_() ->
%%     [ ?_assertEqual( "18f47a30", solve1( "abc" ) ) ].
