-module(problem2016_01).
-export([solve1/1, solve2/1]).

-type turn() :: left | right.
-type dir() :: north | west | south | east.
-type coord() :: { integer(), integer() }.
-type pos() :: { coord(), dir() }.
-type steps() :: non_neg_integer().
-type steps_list() :: list( coord() ).
-type instruction() :: { turn(), steps() }.

-type line() :: { coord(), coord() }.
-type lines() :: list( line() ).

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

-spec do_steps( pos(), steps() ) -> coord().
do_steps( { { X, Y }, north }, Steps ) -> { X, Y - Steps };
do_steps( { { X, Y }, east }, Steps ) -> { X + Steps, Y };
do_steps( { { X, Y }, south },  Steps ) -> { X, Y + Steps };
do_steps( { { X, Y }, west }, Steps ) -> { X - Steps, Y }.

-spec get_steps_list( pos(), steps() ) -> steps_list().
get_steps_list( Pos, Steps ) ->
    lists:map( fun( S ) -> do_steps( Pos, S ) end, lists:seq( 1, Steps ) ).

-spec get_next_pos( instruction(), pos() ) -> pos().
get_next_pos( { Turn, Steps }, { Pos, Face } ) ->
    NewFace = do_turn( Turn, Face ),
    { NewX, NewY } = do_steps( { Pos, NewFace }, Steps ),
    { { NewX, NewY }, NewFace }.

parse_instructions( Input ) ->
    lists:map( fun parse_instruction/1, string:tokens( Input, " ," ) ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Instructions = parse_instructions( Input ),
    StartPos = { { 0, 0 }, north },
    { { X, Y }, _ } = lists:foldl( fun get_next_pos/2,
                                   StartPos,
                                   Instructions ),
    abs( X ) + abs( Y ).

belongs( X, A, B ) ->
    ( X >= min( A, B ) ) andalso ( X =< max( A, B ) ).

-spec intersects( coord(), line() ) -> boolean().
intersects( { X, Y }, { { X1, Y1 }, { X2, Y2 } } ) ->
    belongs( X, X1, X2 ) andalso belongs( Y, Y1, Y2 ).

-spec find_if_already_visited( steps_list(), lines() ) -> coord() | not_found.
find_if_already_visited( StepsList, PrevLines ) ->
    listz:find( fun( Step ) ->
                        lists:any( fun( Line ) ->
                                           intersects( Step, Line )
                                   end, PrevLines)
                end,
                StepsList ).

-spec find_first_intersection( list( instruction() ), pos(), lines() ) -> coord().
find_first_intersection( [ { _, 0 } = Instruction | Rest ], CurrentPos, PrevLines ) ->
    find_first_intersection( Rest, get_next_pos( Instruction, CurrentPos ), PrevLines );
find_first_intersection( [ { Turn, Steps } | Rest ], { Pos, Face }, PrevLines ) ->
    NewFace = do_turn( Turn, Face ),
    StepsList = get_steps_list( { Pos, NewFace }, Steps ),

    case find_if_already_visited( StepsList, PrevLines ) of
        not_found ->
            FirstStep = lists:nth( 1, StepsList ),
            LastStep = lists:last( StepsList ),
            NextPos = { LastStep, NewFace },
            NewLine = { FirstStep, LastStep } ,
            find_first_intersection( Rest, NextPos, [ NewLine | PrevLines ] );
        VisitedPos -> VisitedPos
    end.


-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    Instructions = parse_instructions( Input ),
    { X, Y } = find_first_intersection( Instructions, { { 0, 0 }, north }, [ { { 0, 0 }, { 0, 0 } } ] ),
    abs( X ) + abs( Y ).



-include_lib("eunit/include/eunit.hrl").


solve1_test_() ->
    [ ?_assertEqual( 5, solve1( "R2, L3" ) ),
      ?_assertEqual( 2, solve1( "R2, R2, R2" ) ),
      ?_assertEqual( 12, solve1( "R5, L5, R5, R3" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 4, solve2( "R8, R4, R4, R8" ) ) ].

aux_test_() ->
    [ ?_assert( belongs( 0, -1, 1 ) ),
      ?_assert( belongs( 2, 0, 2 ) ),
      ?_assert( belongs( 2, 2, 0 ) ),
      ?_assertNot( belongs( 3, 0, 2 ) ),
      ?_assertNot( belongs( 3, 2, 0 ) ),
      ?_assert( intersects( { 2, 2 }, { { 0, 2 }, { 4, 2 } } ) ),
      ?_assert( intersects( { 2, 2 }, { { 2, 0 }, { 2, 4 } } ) ),
      ?_assert( intersects( { 0, 2 }, { { 0, 2 }, { 4, 2 } } ) ),
      ?_assert( intersects( { 4, 2 }, { { 0, 2 }, { 4, 2 } } ) ),
      ?_assertNot(intersects( { 3, 3 }, { { 0, 2 }, { 4, 2 } } ) ),
      ?_assertEqual( [ { 2, 1 }, { 3, 1 }, { 4, 1 } ], get_steps_list( { { 1, 1 }, east }, 3 ) ),
      ?_assertEqual( { 1, 0 }, find_if_already_visited( [ { 0, 0 }, { 1, 0 }, { 2, 0 } ], [ { { 5, 5 }, { 5, 10 } }, { { 1, -2 }, { 1, 2 } } ] ) )
    ].
