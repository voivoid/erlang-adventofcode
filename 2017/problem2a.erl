-module(problem2a).
-export([solve/1]).

solve(Input) ->
    CalcRowDiff = fun(Row) ->
       Ints = lists:map( fun list_to_integer/1, string:tokens( Row, " " ) ),
       {Min, Max} = {lists:min( Ints ), lists:max( Ints )},
       Max - Min end,
    RowsDiff = lists:map( CalcRowDiff, string:tokens( Input, "\n" ) ),
    lists:sum( RowsDiff ).
