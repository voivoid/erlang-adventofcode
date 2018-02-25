-module(problem2017_02).
-export([solve1/1, solve2/1]).

-spec solve( string(), fun( ( string() ) -> [ integer() ] ) ) -> integer().
solve( Input, CalcRowFunc ) ->
    CalcRowDiff = fun( Row ) ->
                          Ints = lists:map( fun erlang:list_to_integer/1, string:tokens( Row, " \t" ) ),
                          CalcRowFunc( Ints )
                  end,
    RowsDiff = lists:map( CalcRowDiff, string:tokens( Input, "\n" ) ),
    lists:sum( RowsDiff ).

-spec solve1( string() ) -> integer().
solve1( Input ) ->
    CalcRowDiff = fun( Ints ) ->
                          lists:max( Ints ) - lists:min( Ints )
                  end,
    solve( Input, CalcRowDiff ). 



-spec solve2( string() ) -> integer().
solve2( Input ) ->
    CalcRowDiff = fun( Ints ) ->
                          lists:sum( [ M div N || N <- Ints, M <- lists:delete( N, Ints ), M rem N == 0 ] ) 
                  end,
    solve( Input, CalcRowDiff ).
