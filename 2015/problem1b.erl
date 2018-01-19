-module(problem1b).
-export([solve/1]).

iter(-1, _, Counter) ->
    Counter;
iter(Floor, [40|Input], Counter) -> % '('
    iter(Floor + 1, Input, Counter + 1);
iter(Floor, [41|Input], Counter) -> % ')'
    iter(Floor - 1, Input, Counter + 1);
iter(Floor, [_|Input], Counter) ->
    iter(Floor, Input, Counter).


solve(Input) ->
    iter(0, Input, 0).
