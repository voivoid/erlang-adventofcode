-module(problem1b).
-export([solve/1]).

solve(Input) ->
    Ints = lists:map( fun (X) -> list_to_integer( [X] ) end, Input ),
    {L1, L2} = lists:split( length(Ints) div 2, Ints ),
    F = fun(A, B) -> if A == B -> A; A /= B -> 0 end end,
    lists:sum( lists:zipwith( F, Ints, L2 ++ L1 ) ).
