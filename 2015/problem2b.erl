-module(problem2b).
-export([solve/1]).

calcRibbon(Input) ->
    Dims = lists:map( fun list_to_integer/1, string:tokens( Input, "x" ) ),
    [L,W,H] = Dims,
    [S1, S2, _] = lists:sort(Dims),
    Wrap =  S1 * 2 + S2 * 2,
    Bow = L * W * H,
    Wrap + Bow.

solve(Input) ->
    Lines = string:tokens( Input, "\n" ),
    { _, Sum } = lists:mapfoldl( fun(Line, Sum) -> R = calcRibbon( Line ), { R , R + Sum } end, 0, Lines ),
    Sum.
                            
    
