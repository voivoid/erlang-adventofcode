-module(problem2015_08).
-export([solve1/1, solve2/1]).

%%% COMMON

-spec solve( string(), fun( ( string() ) -> integer() ) ) -> integer().
solve( Input, StringCalculator ) ->
    Lines = string:tokens( Input, "\n" ),
    Diffs = lists:map( StringCalculator, Lines ),
    lists:sum( Diffs ).

%%% PART 1

-spec calc_memory_chars( string(), integer() ) -> integer().
calc_memory_chars( [],                       Acc ) -> Acc;
calc_memory_chars( [ $\\, $"       | Rest ], Acc ) -> calc_memory_chars( Rest, Acc + 1 );
calc_memory_chars( [ $\\, $\\      | Rest ], Acc ) -> calc_memory_chars( Rest, Acc + 1 );
calc_memory_chars( [ $\\, $x, _, _ | Rest ], Acc ) -> calc_memory_chars( Rest, Acc + 1 );
calc_memory_chars( [ $"            | Rest ], Acc ) -> calc_memory_chars( Rest, Acc     );
calc_memory_chars( [ _             | Rest ], Acc ) -> calc_memory_chars( Rest, Acc + 1 ).

-spec solve1( string() ) -> integer().
solve1( Input ) ->
    solve( Input, fun( Line ) -> erlang:length( Line ) - calc_memory_chars( Line, 0 ) end ).

%%% PART 2

-spec calc_new_repr_chars( string(), integer() ) -> integer().
calc_new_repr_chars( [],                       Acc ) -> Acc;
calc_new_repr_chars( [ $\\, $"       | Rest ], Acc ) -> calc_new_repr_chars( Rest, Acc + 4 );
calc_new_repr_chars( [ $\\, $\\      | Rest ], Acc ) -> calc_new_repr_chars( Rest, Acc + 4 );
calc_new_repr_chars( [ $\\, $x, _, _ | Rest ], Acc ) -> calc_new_repr_chars( Rest, Acc + 5 );
calc_new_repr_chars( [ $"            | Rest ], Acc ) -> calc_new_repr_chars( Rest, Acc + 3 );
calc_new_repr_chars( [ _             | Rest ], Acc ) -> calc_new_repr_chars( Rest, Acc + 1 ).

-spec solve2( string() ) -> integer().
solve2( Input ) ->
    solve( Input, fun( Line ) -> calc_new_repr_chars( Line, 0 ) - erlang:length( Line ) end ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

sovle1_test_() ->
    [ ?_assertEqual( 2, solve1( "\"\"" ) ),
      ?_assertEqual( 2, solve1( "\"abc\"" ) ),
      ?_assertEqual( 3, solve1( "\"aaa\"aaa\"" ) ),
      ?_assertEqual( 5, solve1( "\"\\x27\"" ) )
    ].

sovle2_test_() ->
    [ ?_assertEqual( 4, solve2( "\"\"" ) ),
      ?_assertEqual( 4, solve2( "\"abc\"" ) ),
      ?_assertEqual( 6, solve2( "\"aaa\"aaa\"" ) ),
      ?_assertEqual( 5, solve2( "\"\\x27\"" ) )
    ].
