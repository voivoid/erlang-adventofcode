-module(problem2015_14).
-export([solve1/1, solve2/1]).

-type speed() :: non_neg_integer().
-type time() :: non_neg_integer().
-type dist() :: non_neg_integer().
-type score() :: non_neg_integer().

%%% COMMON

-record(deer, { speed = 0 :: speed(), flytime = 0 :: time(), resttime = 0 :: time() }).
-type deer() :: #deer{}.

-spec parse_deer( string() ) -> deer().
parse_deer( Line ) ->
    Filter = [ $  | lists:seq( $0, $9 ) ],
    SpaceSeparatedNumbers = lists:filter( fun( C ) -> lists:member( C, Filter ) end, Line ),
    NumberStrs = string:tokens( SpaceSeparatedNumbers, " " ),
    [ Speed, FlyTime, RestTime ] = lists:map( fun erlang:list_to_integer/1, NumberStrs ),
    #deer{ speed = Speed, flytime = FlyTime, resttime = RestTime }.

-spec parse_input( string() ) -> [ deer() ].
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:map( fun parse_deer/1, Lines ).

-spec calc_distance( deer(), time() ) -> dist().
calc_distance( #deer{ speed = Speed, flytime = FlyTime, resttime = RestTime }, TotalTime ) ->
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

-record(deer_state, { deer = undefined :: deer(), flytime_left = 0 :: time(), resttime_left = 0 :: time(), distance = 0 :: dist(), score = 0 ::score() }).

make_deer_state( #deer{ flytime = FlyTime, resttime = RestTime } = Deer ) ->
    #deer_state{ deer = Deer, flytime_left = FlyTime, resttime_left = RestTime, distance = 0, score = 0 }.

make_deer_states( Deers ) ->
    [ make_deer_state( Deer ) || Deer <- Deers ].

update_deer_distance( Deer = #deer_state{ deer = #deer{ speed = Speed }, flytime_left = FlyTimeLeft, distance = Distance } ) when FlyTimeLeft > 0  ->
    Deer#deer_state{ flytime_left = FlyTimeLeft - 1, distance = Speed + Distance };
update_deer_distance( Deer = #deer_state{ resttime_left = RestTimeLeft } = Deer ) when RestTimeLeft > 0 ->
    Deer#deer_state{ resttime_left = RestTimeLeft - 1 };
update_deer_distance( Deer = #deer_state{ deer = #deer{ flytime = FlyTime, resttime = RestTime }, resttime_left = 0 } ) ->
    Deer#deer_state{ flytime_left = FlyTime, resttime_left = RestTime }.

update_deer_score( DeerStates ) ->
    MaxDistance = lists:max( [ Distance || #deer_state{ distance = Distance } <- DeerStates ] ),
    lists:map( fun ( #deer_state{ distance = Distance, score = Score } = Deer ) when Distance == MaxDistance -> Deer#deer_state{ score = Score + 1 };
                   ( Deer ) -> Deer
               end,
               DeerStates).

update_deer_states( DeerStates ) ->
    UpdatedDistanceDeerStates = lists:map( fun update_deer_distance/1, DeerStates ),
    update_deer_score( UpdatedDistanceDeerStates ).

solve2( Input, Time ) ->
    Deers = parse_input( Input ),
    DeersAfterRace = lists:foldl( fun( _, DeerStates ) ->
                                          update_deer_states( DeerStates )
                                  end,
                                  make_deer_states( Deers ),
                                  lists:seq( 1, Time ) ),
    lists:max( [ Score || #deer_state{ score = Score } <- DeersAfterRace ] ).

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
