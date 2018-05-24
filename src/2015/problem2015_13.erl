-module(problem2015_13).
-export([solve1/1, solve2/1]).

-type neighbour() :: string().
-type neighbours() :: [ neighbour() ].
-type mod() :: integer().
-type neighbour_mod() :: { neighbour(), mod(), neighbour() }.
-type neighbour_mods_map() :: #{ { neighbour(), neighbour() } := mod() }.

%%% COMMON

-spec parse_neighbour_mod( string() ) -> neighbour_mod().
parse_neighbour_mod( Line ) ->
    [ Name, _, Modifier, Points, _, _, _, _, _, _, Neighbour ] = string:tokens( Line, " ." ),
    PointsNum = erlang:list_to_integer( Points ),
    PointsModifier = case Modifier of
        "gain" -> PointsNum;
        "lose" -> -PointsNum
    end,
    { Name, PointsModifier , Neighbour }.

-spec parse_neighbour_mods( string() ) -> neighbour_mods_map().
parse_neighbour_mods( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:foldl( fun( Line, Map ) ->
                         { Name, Mod, Neighbour } = parse_neighbour_mod( Line ),
                         Map#{ { Name, Neighbour } => Mod }
                 end,
                 #{},
                 Lines ).

-spec get_mod( neighbour(), neighbour(), neighbour_mods_map() ) -> mod().
get_mod( Neighbour1, Neighbour2, ModsMap ) ->
    maps:get( { Neighbour1, Neighbour2 }, ModsMap ).

-spec get_neighbours( neighbour_mods_map() ) -> neighbours().
get_neighbours( NeighbourModsMap ) ->
    { Neighbours, _ } = lists:unzip( maps:keys( NeighbourModsMap ) ),
    lists:usort( Neighbours ).

-spec calc_happinness_impl( zipper:zipper( neighbour() ), neighbour_mods_map() ) -> mod().
calc_happinness_impl( NeighboursZipper, NeighbourModsMap ) ->
    Current = zipper:get( NeighboursZipper ),
    Left = zipper:get( zipper:prev( NeighboursZipper ) ),
    Right = zipper:get( zipper:next( NeighboursZipper ) ),

    LeftMod = get_mod( Current, Left, NeighbourModsMap ),
    RightMod = get_mod( Current, Right, NeighbourModsMap ),

    Mod = LeftMod + RightMod,

    case zipper:is_last( NeighboursZipper ) of
        true -> Mod;
        false -> Mod + calc_happinness_impl( zipper:next( NeighboursZipper ), NeighbourModsMap )
    end.

-spec calc_happinness( neighbours(), neighbour_mods_map() ) -> mod().
calc_happinness( Neighbours, NeighbourModsMap ) ->
    NeighboursZipper = zipper:from_list( Neighbours ),
    calc_happinness_impl( NeighboursZipper, NeighbourModsMap ).

solve( NeighbourModsMap ) ->
    Neighbours = get_neighbours( NeighbourModsMap ),
    NeigbhourPermutations = listz:permutations( Neighbours ),
    AllPossibleHappinnessVariants = lists:map( fun( Permutation ) -> calc_happinness( Permutation, NeighbourModsMap ) end, NeigbhourPermutations ),
    lists:max( AllPossibleHappinnessVariants ).

%%% PART 1

-spec solve1( string() ) -> integer().
solve1( Input ) ->
    NeighbourModsMap = parse_neighbour_mods( Input ),
    solve( NeighbourModsMap ).

%%% PART 2

-spec add_me_to_mods_map( neighbour(), neighbour_mods_map() ) -> neighbour_mods_map().
add_me_to_mods_map( MyName, NeighbourModsMap ) ->
    Neighbours = get_neighbours( NeighbourModsMap ),
    lists:foldl(
      fun( Neighbour, AccMap ) ->
              AccMap#{ { Neighbour, MyName } => 0, { MyName, Neighbour } => 0 }
      end,
      NeighbourModsMap,
      Neighbours ).

-spec solve2( string() ) -> integer().
solve2( Input ) ->
    NeighbourModsMap = parse_neighbour_mods( Input ),
    NeighbourModsMapWithMe = add_me_to_mods_map( "me", NeighbourModsMap ),
    solve( NeighbourModsMapWithMe ).


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
