-module(problem1a).
-export([solve/1]).

solve(Input) ->
    lists:foldl( fun(C, Floor) -> if C == $( -> Floor + 1;
                                     C == $) -> Floor - 1;
                                     true    -> Floor
                                  end end,
                 0, Input ).
                                          
                         
