-module(problem2015_10).
-export([solve1/1, solve2/1]).

%%% COMMON

-spec transform( string() ) -> string().
transform( [] ) -> "";
transform( [ Digit | _ ] = Digits ) ->
    { SameDigits, RestDigits } = lists:splitwith( fun( D ) -> D == Digit end, Digits ),
    NumOfSameDigits = erlang:integer_to_list( erlang:length( SameDigits ) ),
    NumOfSameDigits ++ [ Digit ] ++ transform( RestDigits ).

-spec solve( string(), non_neg_integer() ) -> non_neg_integer().
solve( Input, TransformNum ) ->
    erlang:length( lists:foldl( fun( _, Num ) ->
                         transform( Num )
                 end,
                 Input,
                 lists:seq( 1, TransformNum )
               ) ).

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve( Input, 40 ).

%%% PART 2

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    solve( Input, 50 ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [
     ?_assertEqual( "11", transform( "1" ) ),
     ?_assertEqual( "21", transform( "11" ) ),
     ?_assertEqual( "1211", transform( "21" ) ),
     ?_assertEqual( "111221", transform( "1211" ) ),
     ?_assertEqual( "312211", transform( "111221" ) )
    ].
