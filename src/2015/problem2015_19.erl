-module(problem2015_19).
-export([solve1/1]).
-compile([export_all, nowarn_export_all]).

-type molecula() :: string().
-type replacement() :: { molecula(), molecula() }.
-type replacements_map() :: #{ molecula() := molecula() }.

%%% COMMON

-spec is_lowercase( char() ) -> boolean().
is_lowercase( Char ) ->
    Char == string:to_lower( Char ).

-spec parse_replacement( string() ) -> replacement().
parse_replacement( Replacement ) ->
    [ From, To ] = string:tokens( Replacement, " =>" ),
    { From, To }.

-spec parse_ReplacementsMap( [ string() ] ) -> replacements_map().
parse_ReplacementsMap( ReplacementsMap ) ->
    lists:foldl( fun( Replacement, AccMap ) ->
                         { From, To } = parse_replacement( Replacement ),
                         maps:update_with( From, fun( Value ) -> [ To | Value ]  end, [ To ], AccMap )
                 end,
                 #{},
                 ReplacementsMap ).

-spec split_molecula( molecula() ) -> [ molecula() ].
split_molecula( [] ) -> [];
split_molecula( [ M | MS ] ) ->
    { Left, Right } = lists:splitwith( fun is_lowercase/1, MS ),
    [ [ M | Left ] | split_molecula( Right ) ].

-spec parse_input( string() ) -> { [ molecula() ], replacements_map() }.
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    { ReplacementsMap, Molecula } = lists:split( erlang:length( Lines ) - 1, Lines ),
    { split_molecula( string:trim( Molecula ) ), parse_ReplacementsMap( ReplacementsMap ) }.

find_replacement( Molecula, ReplacementsMap ) ->
    maps:get( Molecula, ReplacementsMap, [] ).

molecula_array_to_str( Array ) ->
      array:foldr( fun( _, Molecula, Acc ) ->
                           Molecula ++ Acc
                   end,
                   "",
                   Array ).


replace_moleculas( Moleculas, ReplacementsMap ) ->
    MoleculasArray = array:from_list( Moleculas ),
    IndexedMoleculas = lists:zip( Moleculas, lists:seq( 0, array:size( MoleculasArray ) - 1 ) ),
    [ molecula_array_to_str( array:set( I, ReplacedMolecula, MoleculasArray ) ) || { Molecula, I } <- IndexedMoleculas, ReplacedMolecula <- find_replacement( Molecula, ReplacementsMap ) ].

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    { Moleculas, ReplacementsMap } = parse_input( Input ),
    Replacements = replace_moleculas( Moleculas, ReplacementsMap ),
    erlang:length( lists:usort( Replacements ) ).

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
