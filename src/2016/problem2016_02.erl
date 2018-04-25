-module(problem2016_02).
-export([solve1/1, solve2/1]).

-type instruction() :: $U | $L | $R | $D.
-type instructions() :: list( instruction() ).
-type coord() :: { non_neg_integer(), non_neg_integer() }.
-type button() :: char().
-type buttons_map() :: #{ coord() := button() }.
-type buttons() :: list( button() ).

-spec next_keypad_coord( instruction(), coord() ) -> coord().
next_keypad_coord( $U, { X, Y } ) -> { X, Y - 1 };
next_keypad_coord( $L, { X, Y } ) -> { X - 1, Y };
next_keypad_coord( $R, { X, Y } ) -> { X + 1, Y };
next_keypad_coord( $D, { X, Y } ) -> { X, Y + 1 }.
    
-spec next_button_coord( instructions(), coord(), buttons_map() ) -> coord().
next_button_coord( Line, StartCoord, ButtonsMap ) ->
    lists:foldl( fun ( Cmd, Coord ) ->
                         NextCoord = next_keypad_coord( Cmd, Coord ),
                         case maps:is_key( NextCoord , ButtonsMap ) of
                             true -> NextCoord;
                             false -> Coord
                         end                             
                 end,
                 StartCoord,
                 Line ).
    

-spec solve( string(), buttons_map(), coord() ) -> buttons().
solve( Input, ButtonsMap, StartCoord ) ->
    Lines = string:tokens( Input, "\n " ),
    { _, ResultButtons } =
        lists:foldl( fun( Line, { Coord, ButtonsToPress } ) ->
                             NextButtonCoord = next_button_coord( Line, Coord, ButtonsMap ),
                             NextButton = maps:get( NextButtonCoord, ButtonsMap ),
                             { NextButtonCoord, [ NextButton | ButtonsToPress ] }
                     end,
                     { StartCoord, [] },
                     Lines ),
    lists:reverse( ResultButtons ).
    
-spec solve1( string() ) -> buttons().
solve1( Input ) ->
    ButtonsMap = #{ { 1, 1 } => $1, { 2, 1 } => $2, { 3, 1 } => $3,
                    { 1, 2 } => $4, { 2, 2 } => $5, { 3, 2 } => $6,
                    { 1, 3 } => $7, { 2, 3 } => $8, { 3, 3 } => $9 },
    solve( Input, ButtonsMap, { 2, 2 } ).

-spec solve2( string() ) -> buttons().
solve2( Input ) ->
    ButtonsMap =
        #{                                 { 3, 1 } => $1,
                           { 2, 2 } => $2, { 3, 2 } => $3, { 4, 2 } => $4,
           { 1, 3 } => $5, { 2, 3 } => $6, { 3, 3 } => $7, { 4, 3 } => $8, { 5, 3 } => $9,
                           { 2, 4 } => $A, { 3, 4 } => $B, { 4, 4 } => $C,
                                           { 3, 5 } => $D
                                   },
    solve( Input, ButtonsMap, { 1, 3 } ).


-include_lib("eunit/include/eunit.hrl").

test_input() ->
    "ULL
     RRDDD
     LURDL
     UUUUD".

solve1_test_() ->
    [ ?_assertEqual( "1985", solve1( test_input() ) ) ].

solve2_test_() ->
    [ ?_assertEqual( "5DB3", solve2( test_input() ) ) ].
