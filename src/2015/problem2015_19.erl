-module(problem2015_19).
-export([solve1/1]).
-compile([export_all, nowarn_export_all]).

-type molecula() :: string().
-type replacement() :: { molecula(), molecula() }.
-type replacements() :: #{ molecula() := molecula() }.

%%% COMMON

-spec is_lowercase( char() ) -> boolean().
is_lowercase( Char ) ->
    Char == string:to_lower( Char ).

-spec parse_replacement( string() ) -> replacement().
parse_replacement( Replacement ) ->
    [ From, To ] = string:tokens( Replacement, " =>" ),
    { From, To }.

-spec parse_replacements( [ string() ] ) -> replacements().
parse_replacements( Replacements ) ->
    lists:foldl( fun( Replacement, AccMap ) ->
                         { From, To } = parse_replacement( Replacement ),
                         maps:update_with( From, fun( Value ) -> [ To | Value ]  end, [ To ], AccMap )
                 end,
                 #{},
                 Replacements ).

-spec split_molecula( molecula() ) -> [ molecula() ].
split_molecula( [] ) -> [];
split_molecula( [ M | MS ] ) ->
    { Left, Right } = lists:splitwith( fun is_lowercase/1, MS ),
    [ [ M | Left ] | split_molecula( Right ) ].

-spec parse_input( string() ) -> { [ molecula() ], replacements() }.
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    { Replacements, Molecula } = lists:split( erlang:length( Lines ) - 1, Lines ),
    { split_molecula( string:trim( Molecula ) ), parse_replacements( Replacements ) }.

find_replacements( Molecula, Replacements ) ->
    maps:get( Molecula, Replacements, Molecula ).

replace_moleculas( Moleculas, Replacements ) ->
    [].

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    { Moleculas, Replacements } = parse_input( Input ),
    replace_moleculas( Moleculas, Replacements ).

%%% PART 2

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

test_input_1() ->
    "H => HO
     H => OH
     O => HH

     HOH".

test_input_2() ->
    "H => HO
     H => OH
     O => HH

     HOHOHO".

solve1_test_() ->
    [ ?_assertEqual( 4, solve1( test_input_1() ) ),
      ?_assertEqual( 7, solve1( test_input_2() ) ) ].
