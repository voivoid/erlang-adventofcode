-module(problem2017_09).
-export([solve1/1, solve2/1]).


-type counter() :: non_neg_integer().
-type parser_state() :: bang | garbage | { group, counter() }.
-type state() :: { list( parser_state() ), counter(), counter() }.

-spec process( char(), state() ) -> state().
process( _, { [ bang | States ], S, G } ) ->
    { States, S, G };

process( $!, { States, S, G } ) ->
    { [ bang | States ], S, G };

process( $>, { [ garbage | States ], S, G } ) ->
    { States, S, G };

process( _, { [ garbage | _ ] = States, S, G } ) ->
    { States, S, G + 1 };

process( $<, { States, S, G } ) ->
    { [ garbage | States ], S, G };

process( ${, { [ { group, N } | _ ] = States, S, G } ) ->
    { [ { group, N + 1 } | States ], S, G };
process( $}, { [ { group, N } | States ], S, G } ) ->
    { States, S + N, G };

process( $,, States ) ->
    States.


-spec solve( string() ) -> state().
solve( Input ) ->
    lists:foldl( fun process/2, { [ { group, 0 } ], 0, 0 }, Input ).


-spec solve1( string() ) -> counter().
solve1( Input ) ->
    { _, Score, _ } = solve( Input ),
    Score.

-spec solve2( string() ) -> counter().
solve2( Input ) ->
    { _, _, Garbage } = solve( Input ),
    Garbage.


-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [
     ?_assertEqual( 1, solve1( "{}" ) ),
     ?_assertEqual( 6, solve1( "{{{}}}" ) ),
     ?_assertEqual( 5, solve1( "{{},{}}" ) ),
     ?_assertEqual( 16, solve1( "{{{},{},{{}}}}" ) ),
     ?_assertEqual( 1, solve1( "{<a>,<a>,<a>,<a>}" ) ),
     ?_assertEqual( 9, solve1( "{{<ab>},{<ab>},{<ab>},{<ab>}}" ) ),
     ?_assertEqual( 9, solve1( "{{<!!>},{<!!>},{<!!>},{<!!>}}" ) ),
     ?_assertEqual( 3, solve1( "{{<a!>},{<a!>},{<a!>},{<ab>}}" ) )
    ].

solve2_test_() ->
    [
     ?_assertEqual( 0, solve2( "<>" ) ),
     ?_assertEqual( 17, solve2( "<random characters>" ) ),
     ?_assertEqual( 3, solve2( "<<<<>" ) ),
     ?_assertEqual( 2, solve2( "<{!>}>" ) ),
     ?_assertEqual( 0, solve2( "<!!>" ) ),
     ?_assertEqual( 0, solve2( "<!!!>>" ) ),
     ?_assertEqual( 10, solve2( "<{o\"i!a,<{i<a>" ) )
    ].
