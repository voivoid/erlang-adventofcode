-module(problem2016_01).
-export([solve1/1, solve2/1, intersects/2]).

-type turn() :: left | right.
-type dir() :: north | west | south | east.
-type coord() :: { integer(), integer() }.
-type pos() :: { coord(), dir() }.
-type instruction() :: { turn(), non_neg_integer() }.

-type move() :: { coord(), coord() }.
-type moves() :: list( move() ).

-spec parse_instruction( [ char() ] ) -> instruction().
parse_instruction( [ $L | Steps ] ) ->
    { left, list_to_integer( Steps ) };
parse_instruction( [ $R | Steps ] ) ->
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

-spec get_next_pos( instruction(), pos() ) -> pos().
get_next_pos( { Turn, Steps }, { { X, Y }, Face } ) ->
    NewFace = do_turn( Turn, Face ),
    { NewX, NewY } = do_steps( NewFace, { X, Y }, Steps ),
    { { NewX, NewY }, NewFace }.

parse_instructions( Input ) ->
    lists:map( fun parse_instruction/1, string:tokens( Input, " ," ) ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Instructions = parse_instructions( Input ),
    { { X, Y }, _ } = lists:foldl( fun get_next_pos/2,
                                   { { 0, 0 }, north },
                                   Instructions ),
    abs( X ) + abs( Y ).



-spec intersects( move(), move() ) -> no | coord().
intersects( M1, M2 ) ->
    case intersects_impl( M1, M2 ) of
        no -> intersects_impl( M2, M1 );
        Intersection -> Intersection
    end.

intersects_impl( { { X1, Y }, { X2, Y } }, { { X, Y1 }, { X, Y2 } } ) when ( X >= X1 ) and ( X =< X2 ) and ( Y >= Y1 ) and ( Y =< Y2 ) -> { X, Y };
intersects_impl( _, _ ) -> no.

-spec do_instructions( instruction(), pos(), moves() ) -> pos().
do_instructions( [ Instruction | Rest ], CurrentPos, PrevMoves ) ->
    NextPos = get_next_pos( Instruction, CurrentPos ),
    { { P1, _ }, { P2, _ } } = { CurrentPos, NextPos },
    NewMove = { P1, P2 },
    io:format(user, "~w~n", [ { NewMove } ] ),
    
    case listz:find( fun( Move ) -> intersects( Move, NewMove ) /= no end, PrevMoves ) of
        not_found -> 
            NewMoves = [ NewMove | PrevMoves ],
            do_instructions( Rest, NextPos, NewMoves );
        InterMove -> intersects( InterMove, NewMove )
    end.
    
-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    Instructions = parse_instructions( Input ),
    { X, Y } = do_instructions( Instructions, { { 0, 0 }, north }, [] ),
    io:format(user, "~w", [ { X, Y } ] ),
    abs( X ) + abs( Y ).

    


-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 5, solve1( "R2, L3" ) ),
      ?_assertEqual( 2, solve1( "R2, R2, R2" ) ),
      ?_assertEqual( 12, solve1( "R5, L5, R5, R3" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 4, solve2( "R8, R4, R4, R8" ) ) ].
