-module(problem2a).
-export([solve/1]).

calcArea(Input) ->
    Dims = lists:map( fun list_to_integer/1, string:tokens( Input, "x" ) ),
    [L,W,H] = Dims,
    Sides = [ L * W, W * H, H * L ],
    Area = lists:sum( lists:map( fun(X) -> X * 2 end, Sides ) ),
    SmallestSide = lists:min( Sides ),
    Area + SmallestSide.

solve(Input) ->
    Lines = string:tokens( Input, "\n" ),
    Sum = lists:foldl( fun(Line, Sum) -> R = calcArea( Line ), R + Sum end, 0, Lines ),
    Sum.
                            
    
