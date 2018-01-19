-module(problem4a).
-export([solve/1]).

solve(Input) ->
    Lines = string:tokens( Input, "\n" ),
    IsValidLine = fun (Line) ->
        Tokens = string:tokens( Line, " " ),
        TokensSet = sets:from_list(Tokens),
        length(Tokens) == sets:size(TokensSet) end,
    length( lists:filter( IsValidLine, Lines) ).    
