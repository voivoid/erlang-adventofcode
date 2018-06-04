-module(problem2015_17).
-export([solve1/1]).
-compile([export_all, nowarn_export_all]).

-type volume() :: non_neg_integer().
-type container() :: volume().
-type containers() :: [ container() ].

%%% COMMON

-spec parse_containers( string() ) -> containers().
parse_containers( Input ) ->
    lists:map( fun erlang:list_to_integer/1, string:tokens( Input, " \n" ) ).

-spec calc_containers_volume( containers() ) -> volume().
calc_containers_volume( Containers ) ->
    lists:sum( Containers ).

-spec find_combinations( containers(), volume() ) -> [ containers() ].
find_combinations( _, Volume ) when Volume =< 0 -> [ [] ];
find_combinations( [], _ ) -> [];
find_combinations( [ C | CS ], Volume ) ->
    [ [ C | Combination ] || Combination <- find_combinations( CS, Volume - C ), calc_containers_volume( Combination ) + C == Volume ] ++ find_combinations( CS, Volume ).

-spec solve( string(), volume() ) -> [ containers() ].
solve( Input, Volume ) ->
    Containers = lists:sort( parse_containers( Input ) ),
    find_combinations( Containers, Volume ).

%%% PART 1

-spec solve1( string(), volume() ) -> non_neg_integer().
solve1( Input, Volume ) ->
    Combinations = solve( Input, Volume ),
    erlang:length( Combinations ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve1( Input, 150 ).

%%% PART 2

-spec solve2( string(), volume() ) -> non_neg_integer().
solve2( Input, Volume ) ->
    Combinations = solve( Input, Volume ),
    CombinationMinLength = lists:min( [ erlang:length( C ) || C <- Combinations ] ),
    MinLengthCombinations = [ C || C <- Combinations, erlang:length( C ) == CombinationMinLength ],
    erlang:length( MinLengthCombinations ).

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    solve2( Input, 150 ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

test_input() ->
    "20
     15
     10
     5
     5".

solve1_test_() ->
    [ ?_assertEqual( 4, solve1( test_input(), 25 ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 3, solve2( test_input(), 25 ) ) ].
