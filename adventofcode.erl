-module(adventofcode).
-compile(export_all).

problem1a(Xs) ->
    Nums = lists:map(fun (X) -> list_to_integer([X]) end, Xs),
    F = fun(A, B) -> if A == B -> A; A /= B -> 0 end end,
    lists:sum( lists:zipwith( F, Nums, tl(Nums) ++ [hd(Nums)] ) ).

problem1b(Xs) ->
    Nums = lists:map(fun (X) -> list_to_integer([X]) end, Xs),
    {L1, L2} = lists:split(length(Nums) div  2, Nums),
    F = fun(A, B) -> if A == B -> A; A /= B -> 0 end end,
    lists:sum( lists:zipwith( F, Nums, L2 ++ L1 ) ).
