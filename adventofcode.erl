-module(adventofcode).
-compile(export_all).

% https://adventofcode.com problems

problem1a(Input) ->
    Ints = lists:map(fun (X) -> list_to_integer([X]) end, Input),
    F = fun(A, B) -> if A == B -> A; A /= B -> 0 end end,
    lists:sum( lists:zipwith( F, Ints, tl(Ints) ++ [hd(Ints)] ) ).

problem1b(Input) ->
    Ints = lists:map(fun (X) -> list_to_integer([X]) end, Input),
    {L1, L2} = lists:split(length(Ints) div  2, Ints),
    F = fun(A, B) -> if A == B -> A; A /= B -> 0 end end,
    lists:sum( lists:zipwith( F, Ints, L2 ++ L1 ) ).

problem2a(Input) ->
    CalcRowDiff = fun(Row) ->
       Ints = lists:map(fun list_to_integer/1, string:tokens(Row, " ")),
       {Min, Max} = {lists:min(Ints), lists:max(Ints)},
       Max - Min end,
    RowsDiff = lists:map(CalcRowDiff, string:tokens(Input, "\n")),
    lists:sum(RowsDiff).
