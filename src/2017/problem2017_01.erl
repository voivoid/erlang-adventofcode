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


-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 3 , solve1( "1122" ) ),
      ?_assertEqual( 4 , solve1( "1111" ) ),
      ?_assertEqual( 0 , solve1( "1234" ) ),
      ?_assertEqual( 9 , solve1( "91212129" ) ) ].


solve2_test_() ->
    [ ?_assertEqual( 6, solve2( "1212" ) ),
      ?_assertEqual( 0, solve2( "1221" ) ),
      ?_assertEqual( 4, solve2( "123425" ) ),
      ?_assertEqual( 12, solve2( "123123" ) ),
      ?_assertEqual( 4, solve2( "12131415" ) ) ].
