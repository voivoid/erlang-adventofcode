-module(problem2016_01).
-export([solve1/1, solve2/1]).

-type chars() :: $L | $R.
-type turn() :: left | right.
-type dir() :: north | west | south | east.
-type coord() :: { integer(), integer() }.
-type pos() :: { coord(), dir() }.
-type instruction() :: { turn(), non_neg_integer() }.

-spec parse_turn( [ chars() ] ) -> { turn(), integer() }.
parse_turn( [ $L | Steps ] ) ->
    { left, list_to_integer( Steps ) };
parse_turn( [ $R | Steps ] ) ->
    { right, list_to_integer( Steps ) }.

-spec do_turn( turn(), dir() ) -> dir().
do_turn( left, north ) -> west;
do_turn( left, west ) -> south;
do_turn( left, south ) -> east;
do_turn( left, east ) -> north;
do_turn( right, north ) -> east;
do_turn( right, west ) -> north;
do_turn( right, south ) -> west;
do_turn( right, east ) -> south.

-spec do_steps( dir(), coord(), non_neg_integer() ) -> coord().
do_steps( north, { X, Y }, Steps ) -> { X, Y - Steps };
do_steps( east,  { X, Y }, Steps ) -> { X + Steps, Y };
do_steps( south, { X, Y }, Steps ) -> { X, Y + Steps };
do_steps( west,  { X, Y }, Steps ) -> { X - Steps, Y }.

-spec do_instruction( instruction(), pos() ) -> pos().
do_instruction( { Turn, Steps }, { { X, Y }, Face } ) ->
    NewFace = do_turn( Turn, Face ),
    { NewX, NewY } = do_steps( NewFace, { X, Y }, Steps ),
    { { NewX, NewY }, NewFace }.

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Turns = string:tokens( Input, " ," ),
    { { X, Y }, _ } = lists:foldl( fun( Turn, CurrentCoord ) -> ParsedTurn = parse_turn( Turn ),
                                                                do_instruction( ParsedTurn, CurrentCoord )
                                   end,
                                   { { 0, 0 }, north },
                                   Turns ),
    abs( X ) + abs( Y ).


-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    0.
