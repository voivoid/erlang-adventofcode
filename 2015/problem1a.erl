-module(problem1a).
-export([solve/1]).

solve(Input) ->
    lists:foldl( fun(C, Floor) -> if C == 40 -> Floor + 1;
                                     C == 41 -> Floor - 1;
                                     true    -> Floor
                                  end end,
                 0, Input ).
                                          
                         
