-module(problem2015_19).
-export([solve1/1, solve2/1]).
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

-spec find_replacements( molecula(), replacements_map() ) -> [ molecula() ].
find_replacements( Molecula, ReplacementsMap ) ->
    maps:get( Molecula, ReplacementsMap, [] ).

-spec replace_moleculas( [ molecula() ], replacements_map() ) -> [ molecula() ].
replace_moleculas( Moleculas, ReplacementsMap ) ->
    IndexedMoleculas = lists:zip( Moleculas, lists:seq( 1, erlang:length( Moleculas ) ) ),
    [ listz:set_elem( ReplacedMolecula, I, Moleculas ) || { Molecula, I } <- IndexedMoleculas, ReplacedMolecula <- find_replacements( Molecula, ReplacementsMap ) ].

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    { Moleculas, ReplacementsMap } = parse_input( Input ),
    Replacements = replace_moleculas( Moleculas, ReplacementsMap ),
    erlang:length( lists:usort( [ lists:append( R ) || R <- Replacements ] ) ).

%%% PART 2

transmutate_moleculas( [], _, _, _ ) -> error( fatal );
transmutate_moleculas( Moleculas, ExpectedMolecula, ReplacementsMap, Step ) ->
    io:format(user, "~p ~p~n", [ Moleculas, Step ] ),
    case lists:member( ExpectedMolecula, Moleculas ) of
        true -> Step;
        false ->
            Replacements = [ ReplacedMolecula || Molecula <- Moleculas, ReplacedMolecula <- replace_moleculas( Molecula, ReplacementsMap ) ],
            transmutate_moleculas( Replacements, ExpectedMolecula, ReplacementsMap, Step + 1 )
    end.

solve2( Input ) ->
    { Moleculas, ReplacementsMap } = parse_input( Input ),
    ExpectedMolecula = Moleculas,
    transmutate_moleculas( [ [ "e" ] ], ExpectedMolecula, ReplacementsMap, 0 ),
    0.

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

test_input_1() ->
    "e => H
     e => O
     H => HO
     H => OH
     O => HH

     HOH".

test_input_2() ->
    "e => H
     e => O
     H => HO
     H => OH
     O => HH

     HOHOHO".

solve1_test_() ->
    [ ?_assertEqual( 4, solve1( test_input_1() ) ),
      ?_assertEqual( 7, solve1( test_input_2() ) ) ].

%solve2_test_() ->
%    [ ?_assertEqual( 3, solve2( test_input_1() ) )
%      ?_assertEqual( 6, solve2( test_input_2() ) )
%    ].
