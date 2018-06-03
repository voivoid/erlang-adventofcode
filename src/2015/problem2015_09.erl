-module(problem2015_09).
-export([solve1/1, solve2/1]).
-compile([export_all, nowarn_export_all]).

%%% COMMON

-type location() :: string().
-type distance() :: non_neg_integer().
-type path() :: { location(), location(), distance() }.
-type path_map() :: #{ { location(), location() } := distance() }.
-type route() :: [ location() ].
-type routes() :: [ route() ].

-spec parse_path( string() ) -> path().
parse_path( Line ) ->
    [ From, _, To, Distance ] = string:tokens( Line, " =" ),
    { From, To, erlang:list_to_integer( Distance ) }.

-spec parse_paths( string() ) -> path_map().
parse_paths( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:foldl( fun( Line, Acc ) ->
                         { From, To, Distance } = parse_path( Line ),
                         Acc#{ { From, To } => Distance, { To, From } => Distance }
                 end,
                 #{},
                 Lines ).

-spec get_locations( path_map() ) -> [ location() ].
get_locations( PathMap ) ->
    { Locations, _ } = lists:unzip( maps:keys( PathMap ) ),
    lists:usort( Locations ).

-spec generate_routes( path_map() ) -> routes().
generate_routes( PathMap ) ->
    Locations = get_locations( PathMap ),
    listz:permutations( Locations ).

-spec get_distance( location(), location(), path_map() ) -> distance().
get_distance( From, To, PathMap ) ->
    maps:get( { From, To }, PathMap ).

-spec calc_route_distance( routes(), path_map() ) -> distance().
calc_route_distance( [ From, To ], PathMap ) ->
    get_distance( From, To, PathMap );
calc_route_distance( [ From, To | Rest ], PathMap ) ->
    get_distance( From, To, PathMap ) + calc_route_distance( [ To | Rest ], PathMap ).

-spec solve( string(), fun( ( distance() ) -> distance() ) ) -> distance().
solve( Input, FindFunc ) ->
    PathMap = parse_paths( Input ),
    Routes = generate_routes( PathMap ),
    FindFunc( lists:map( fun( Route ) -> calc_route_distance( Route, PathMap ) end, Routes ) ).

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve( Input, fun lists:min/1 ).

%%% PART 2

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    solve( Input, fun lists:max/1 ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

test_input() ->
    "London to Dublin = 464
     London to Belfast = 518
     Dublin to Belfast = 141".

solve1_test_() ->
    [ ?_assertEqual( 605, solve1( test_input() ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 982, solve2( test_input() ) ) ].
