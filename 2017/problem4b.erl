-module(problem4b).
-export([solve/1]).

solve(Input) ->
    Lines = string:tokens( Input, "\n" ),
    IsValidLine = fun(Line) ->
        Tokens = string:tokens( Line, " " ),
        Sorted = lists:map( fun lists:sort/1, Tokens ),
        length(Sorted) == length( lists:usort( Sorted ) ) end,
    length( lists:filter( IsValidLine, Lines ) ).
                           
