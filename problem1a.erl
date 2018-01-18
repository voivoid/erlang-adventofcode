-module(problem1a).
-export([solve/1]).

solve(Input) ->
    Ints = lists:map( fun (X) -> list_to_integer( [X] ) end, Input ),
    F = fun(A, B) -> if A == B -> A; A /= B -> 0 end end,
    lists:sum( lists:zipwith( F, Ints, tl( Ints ) ++ [hd( Ints )] ) ).
