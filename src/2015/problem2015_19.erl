-module(problem2015_19).
-export([solve1/1, solve2/1]).
-compile([export_all, nowarn_export_all]).

-type elem() :: string().
-type molecula() :: [ elem() ].
-type replacement() :: { elem(), molecula() }.
-type replacements_map() :: #{ elem() := molecula() }.

%%% COMMON

-spec is_lowercase( char() ) -> boolean().
is_lowercase( Char ) ->
    Char == string:to_lower( Char ).

-spec parse_replacement( string() ) -> replacement().
parse_replacement( Replacement ) ->
    [ From, To ] = string:tokens( Replacement, " =>" ),
    { From, To }.

-spec parse_replacements_map( [ string() ] ) -> replacements_map().
parse_replacements_map( ReplacementsMap ) ->
    lists:foldl( fun( Replacement, AccMap ) ->
                         { FromElem, ToStr } = parse_replacement( Replacement ),
                         To = parse_molecula( ToStr ),
                         maps:update_with( FromElem, fun( Value ) -> [ To | Value ]  end, [ To ], AccMap )
                 end,
                 #{},
                 ReplacementsMap ).

-spec parse_molecula( string() ) -> molecula().
parse_molecula( [] ) -> [];
parse_molecula( [ M | MS ] ) ->
    { Left, Right } = lists:splitwith( fun is_lowercase/1, MS ),
    [ [ M | Left ] | parse_molecula( Right ) ].

-spec parse_input( string() ) -> { [ molecula() ], replacements_map() }.
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    { ReplacementsMap, Molecula } = lists:split( erlang:length( Lines ) - 1, Lines ),
    { parse_molecula( string:trim( Molecula ) ), parse_replacements_map( ReplacementsMap ) }.

-spec find_elem_replacements( elem(), replacements_map() ) -> [ elem() ].
find_elem_replacements( Molecula, ReplacementsMap ) ->
    maps:get( Molecula, ReplacementsMap, [] ).

-spec replace_elems_in_molecula( molecula(), replacements_map() ) -> [ molecula() ].
replace_elems_in_molecula( Molecula, ReplacementsMap ) ->
    IndexedElems = lists:zip( Molecula, lists:seq( 1, erlang:length( Molecula ) ) ),
    [ listz:replace( ReplacedElem, I, Molecula ) || { Elem, I } <- IndexedElems, ReplacedElem <- find_elem_replacements( Elem, ReplacementsMap ) ].

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    { Molecula, ReplacementsMap } = parse_input( Input ),
    Replacements = replace_elems_in_molecula( Molecula, ReplacementsMap ),
    erlang:length( lists:usort( [ lists:append( R ) || R <- Replacements ] ) ).

%%% PART 2

transmutate_moleculas( [], _, _, _ ) -> error( xxx );
transmutate_moleculas( Moleculas, ExpectedMolecula, ReplacementsMap, Step ) ->
    case lists:member( ExpectedMolecula, Moleculas ) of
        true -> Step;
        false ->
            Replacements = [ ReplacedMolecula || Molecula <- Moleculas, ReplacedMolecula <- replace_elems_in_molecula( Molecula, ReplacementsMap ) ],
            transmutate_moleculas( Replacements, ExpectedMolecula, ReplacementsMap, Step + 1 )
    end.

solve2( Input ) ->
    { ExpectedMolecula, ReplacementsMap } = parse_input( Input ),
    transmutate_moleculas( [ [ "e" ] ], ExpectedMolecula, ReplacementsMap, 0 ).

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
      ?_assertEqual( 7, solve1( test_input_2() ) )
    ].

solve2_test_() ->
    [ ?_assertEqual( 3, solve2( test_input_1() ) ),
      ?_assertEqual( 6, solve2( test_input_2() ) )
    ].
