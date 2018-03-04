-module(problem2017_11).
-export([solve1/1, solve2/1]).

-type distance() :: non_neg_integer().
-type coord() :: integer().
-type pos() :: { coord(), coord() }.
-type step() :: string().
-type state() :: { pos(), distance() }.

-spec make_step( step(), pos() ) -> pos().
make_step( "n",  { X, Y } ) -> { X    , Y + 2 };
make_step( "ne", { X, Y } ) -> { X + 1, Y + 1 };
make_step( "se", { X, Y } ) -> { X + 1, Y - 1 };
make_step( "s",  { X, Y } ) -> { X    , Y - 2 };
make_step( "sw", { X, Y } ) -> { X - 1, Y - 1 };
make_step( "nw", { X, Y } ) -> { X - 1, Y + 1 }.

-spec calc_min_distance( pos() ) -> distance().
calc_min_distance( { X, Y } ) ->
    AbsX = abs( X ),
    AbsY = abs( Y ),
    AbsX + ( ( AbsY - AbsX ) div 2 ).

-spec calc_next_pos( step(), state() ) -> state().
calc_next_pos( Step, { Pos, Max } ) ->
    NewPos = make_step( Step, Pos ),
    NewMax = max( Max, calc_min_distance( NewPos ) ),
    { NewPos, NewMax }.
    

-spec solve( string() ) -> state().
solve( Input ) ->
    Steps = string:tokens( Input, "," ),
    lists:foldl( fun calc_next_pos/2,
                 { { 0, 0 }, 0 },
                 Steps ).


-spec solve1( string() ) -> distance().
solve1( Input ) ->
    { FinalPos, _ } = solve( Input ),
    calc_min_distance( FinalPos ).

-spec solve2( string() ) -> distance().
solve2( Input ) ->
    { _, MaxDistance } = solve( Input ),
    MaxDistance.



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 3, solve1( "ne,ne,ne" ) ),
      ?_assertEqual( 0, solve1( "ne,ne,sw,sw" ) ),
      ?_assertEqual( 2, solve1( "ne,ne,s,s" ) ),
      ?_assertEqual( 3, solve1( "se,sw,se,sw,sw" ) ) ].
