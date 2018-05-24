-module(problem2015_14).
-export([solve1/1]).

-type speed() :: non_neg_integer().
-type time() :: non_neg_integer().
-type dist() :: non_neg_integer().
-type deer() :: { speed(), time() }.

%%% COMMON

-spec parse_deer( string() ) -> deer().
parse_deer( Line ) ->
    

-spec parse_input( string() ) -> [ deer() ].
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:map( fun parse_deer/1, Lines ).

-spec calc_distance( deer(), time() ) -> dist().
calc_distance( Deer, Time ) ->
    0.

-spec solve( string(), time() ) -> dist().
solve( Input, Time ) ->
    Deers = parse_input( Input ),
    Distances = lists:map( fun( Deer ) -> calc_distance( Deer, Time ) end, Deers ),
    lists:max( Distances ).

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve( Input, 2503 ).

%%% TESTS

-include_lib( "eunit/include/eunit.hrl" ).

test_input() ->
    "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
     Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.".

solve1_test_() ->
    [ ?_assertEqual( 1120, solve( test_input(), 1000 ) ) ].
