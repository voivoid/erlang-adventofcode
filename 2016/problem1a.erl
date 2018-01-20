-module(problem1a).
-export([solve/1]).

parseTurn( [76 | Steps ] ) -> % 'L'
    { left, list_to_integer( Steps ) };
parseTurn( [82 | Steps ] ) -> % 'R'
    { right, list_to_integer( Steps ) }.

doTurn( left, north ) -> west;
doTurn( left, west ) -> south;
doTurn( left, south ) -> east;
doTurn( left, east ) -> north;
doTurn( right, north ) -> east;
doTurn( right, west ) -> north;
doTurn( right, south ) -> west;
doTurn( right, east ) -> south.

doSteps( north, {X, Y}, Steps ) -> { X, Y - Steps };
doSteps( east,  {X, Y}, Steps ) -> { X + Steps, Y };
doSteps( south, {X, Y}, Steps ) -> { X, Y + Steps };
doSteps( west,  {X, Y}, Steps ) -> { X - Steps, Y }.

doInstruction( { Turn, Steps }, { X, Y, Face } ) ->
    NewFace = doTurn( Turn, Face ),
    { NewX, NewY } = doSteps( NewFace, { X, Y }, Steps ),
    { NewX, NewY, NewFace }.

solve(Input) ->
    Turns = string:tokens( Input, " ," ),
    { _, { X, Y, _ } } = lists:mapfoldl( fun( Turn, CurrentCoord ) -> ParsedTurn = parseTurn( Turn ),
                                                                      { ParsedTurn, doInstruction( ParsedTurn, CurrentCoord ) }
                                                                      end,
                                         { 0, 0, north },
                                         Turns ),
    abs( X ) + abs( Y ).
