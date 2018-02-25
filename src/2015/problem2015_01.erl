-module(problem2015_01).
-export([solve1/1, solve2/1]).

-spec solve1( string() ) -> integer().
solve1( Input ) ->
    lists:foldl( fun( C, Floor ) -> if C == $( -> Floor + 1;
                                       C == $) -> Floor - 1;
                                       true    -> Floor
                                    end end,
                 0, Input ).

-spec iter( integer(), [ char() ], integer() ) -> non_neg_integer().                         
iter( -1, _, Counter ) ->
    Counter;
iter( Floor, [ $( | Input ], Counter ) ->
    iter( Floor + 1, Input, Counter + 1 );
iter( Floor, [ $) | Input ], Counter ) ->
    iter( Floor - 1, Input, Counter + 1 );
iter( Floor, [ _ | Input ], Counter ) ->
    iter( Floor, Input, Counter ).

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    iter( 0, Input, 0 ).
