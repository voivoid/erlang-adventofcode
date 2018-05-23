-module(problem2015_13).
-export([solve1/1]).

-type name() :: string().
-type modifier() :: integer().
-type neighbour_mod() :: { name(), modifier(), name() }.

%%% COMMON

-spec parse_neighbours( string() ) -> neighbour_mod().
parse_neighbours( Line ) ->
    [ Name, _, Modifier, Points, _, _, _, _, _, _, Neighbour ] = string:tokens( Line, " ." ),
    PointsNum = erlang:list_to_integer( Points ),
    PointsModifier = case Modifier of
        "gain" -> PointsNum;
        "lose" -> -PointsNum
    end,
    { Name, PointsModifier , Neighbour }.

-spec parse_input( string() ) -> [ neighbour_mod() ].
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:map( fun parse_neighbours/1, Lines ).

%%% PART 1

solve1( Input ) ->
    _Neighbours = parse_input( Input ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

test_input() -> "
    Alice would gain 54 happiness units by sitting next to Bob.
    Alice would lose 79 happiness units by sitting next to Carol.
    Alice would lose 2 happiness units by sitting next to David.
    Bob would gain 83 happiness units by sitting next to Alice.
    Bob would lose 7 happiness units by sitting next to Carol.
    Bob would lose 63 happiness units by sitting next to David.
    Carol would lose 62 happiness units by sitting next to Alice.
    Carol would gain 60 happiness units by sitting next to Bob.
    Carol would gain 55 happiness units by sitting next to David.
    David would gain 46 happiness units by sitting next to Alice.
    David would lose 7 happiness units by sitting next to Bob.
    David would gain 41 happiness units by sitting next to Carol.".

solve1_test_() ->
    [ ?_assertEqual( 330, solve1( test_input() ) ) ].
