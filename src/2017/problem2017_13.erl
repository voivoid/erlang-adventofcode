-module(problem2017_13).
-export([solve1/1, solve2/1]).

-type depth() :: non_neg_integer().
-type range() :: non_neg_integer().
-type layer() :: { depth(), range() }.
-type layers() :: list( layer() ).
-type severity() :: non_neg_integer().
-type delay() :: non_neg_integer().

-spec parse_layers( string() ) -> layers().
parse_layers( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:map( fun( Line ) ->
                       [ Depth, Range ] = string:tokens( Line, ": " ),
                       { erlang:list_to_integer( Depth ), erlang:list_to_integer( Range ) }
               end, Lines ).

-spec calc_severity( layer() ) -> severity().
calc_severity( { Depth, Range } ) ->
    IsZeroPos = Depth rem ( Range + ( Range - 2 ) ) == 0,
    if IsZeroPos -> Depth * Range;
       not IsZeroPos -> 0
    end.
    
-spec calc_total_severity( layers() ) -> severity().
calc_total_severity( Layers ) ->
    Severities = lists:map( fun calc_severity/1, Layers ),
    lists:sum( Severities ).

-spec check_delay( delay(), layers() ) -> boolean().
check_delay( Delay, Layers ) ->
    DelayedLayers = lists:map( fun( { Depth, Range } ) -> { Depth + Delay, Range } end, Layers ),
    calc_total_severity( DelayedLayers ) == 0.

-spec calc_min_delay( delay(), layers() ) -> delay().
calc_min_delay( Delay, Layers ) ->
    IsGoodDelay = check_delay( Delay, Layers ),
    if IsGoodDelay -> Delay;
       not IsGoodDelay -> calc_min_delay( Delay + 1, Layers )
    end.
    
-spec solve1( string() ) -> severity().
solve1( Input ) ->
    Layers = parse_layers( Input ),
    calc_total_severity( Layers ).
    
-spec solve2( string() ) -> delay().
solve2( Input ) ->
    Layers = parse_layers( Input ),
    calc_min_delay( 0, Layers ).


-include_lib("eunit/include/eunit.hrl").


test_input() ->
    "0: 3
     1: 2
     4: 4
     6: 4".

solve1_test_() ->
    [ ?_assertEqual( 24, solve1( test_input() ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 10, solve2( test_input() ) ) ].
