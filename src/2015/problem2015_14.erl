-module(problem2015_14).
-export([solve1/1, solve2/1]).

-type speed() :: non_neg_integer().
-type time() :: non_neg_integer().
-type dist() :: non_neg_integer().
-type deer() :: { speed(), time(), time() }.

%%% COMMON

-spec parse_deer( string() ) -> deer().
parse_deer( Line ) ->
    Digits = [ $  | lists:seq( $0, $9 ) ],
    SpaceSeparatedNumbers = lists:filter( fun( C ) -> lists:member( C, Digits ) end, Line ),
    NumberStrs = string:tokens( SpaceSeparatedNumbers, " " ),
    [ Speed, FlyTime, RestTime ] = lists:map( fun erlang:list_to_integer/1, NumberStrs ),
    { Speed, FlyTime, RestTime }.

-spec parse_input( string() ) -> [ deer() ].
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:map( fun parse_deer/1, Lines ).

-spec calc_distance( deer(), time() ) -> dist().
calc_distance( { Speed, FlyTime, RestTime }, TotalTime ) ->
    Period = FlyTime + RestTime,
    PeriodsNum = TotalTime div Period,
    TimeLeft = TotalTime rem Period,
    Speed * ( PeriodsNum * FlyTime + erlang:min( FlyTime, TimeLeft ) ).

%%% PART 1

-spec solve1( string(), time() ) -> dist().
solve1( Input, Time ) ->
    Deers = parse_input( Input ),
    Distances = lists:map( fun( Deer ) -> calc_distance( Deer, Time ) end, Deers ),
    lists:max( Distances ).


-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve1( Input, 2503 ).

%%% PART 2

solve2( _Input, _Time ) ->
    0.

solve2( Input ) ->
    solve2( Input, 2503 ).

%%% TESTS

-include_lib( "eunit/include/eunit.hrl" ).

test_input() ->
    "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
     Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.".

solve1_test_() ->
    [ ?_assertEqual( 1120, solve1( test_input(), 1000 ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 689, solve2( test_input(), 1000 ) ) ].
