-module(problem2017_01).
-export([solve1/1, solve2/1]).

-spec solve( nonempty_string(), nonempty_string() ) -> integer().
solve( Input, ShiftedInput ) ->
    F = fun ( A, A ) -> erlang:list_to_integer( [ A ] );
            ( _, _ ) -> 0
        end,
    lists:sum( lists:zipwith( F, Input, ShiftedInput ) ).
    
-spec solve1( nonempty_string() ) -> integer().
solve1( Input ) ->
    solve( Input, tl( Input ) ++ [ hd( Input ) ] ).


-spec solve2( nonempty_string() ) -> integer().
solve2( Input ) ->
    { L2, L1 } = lists:split( length( Input ) div 2, Input ),
    solve( Input, L1 ++ L2 ).
