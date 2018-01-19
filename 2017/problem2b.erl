-module(problem2b).
-export([solve/1]).

solve(Input) ->
    CalcRowDiv = fun(Row) ->
        Ints = lists:map( fun list_to_integer/1, string:tokens( Row, " " ) ),
        lists:sum( [ M div N || N <- Ints, M <- lists:delete( N, Ints ), M rem N == 0 ] ) end,
    RowsDiv = lists:map( CalcRowDiv, string:tokens( Input, "\n" ) ),
    lists:sum( RowsDiv ).
