-module(problem2017_03).
-export([solve1/1]).

-spec calc( integer(), integer() ) -> integer().
calc( Offset, OddSqrt ) ->
    Rem = Offset rem OddSqrt,
    HalfSqrt = OddSqrt div 2,
    HalfSqrt + erlang:abs( Rem + 1 - HalfSqrt ).

-spec calc( integer() ) -> integer().
calc( 1 ) -> 0;
calc( N ) ->
    Sqrt = erlang:ceil( math:sqrt( N ) ),
    OddSqrt = case Sqrt rem 2 of
                  0 -> Sqrt;
                  1 -> Sqrt - 1
              end,
    PrevSqr = erlang:trunc( math:pow( OddSqrt - 1, 2 ) ),
    Offset = N - ( PrevSqr + 1 ),
    calc( Offset, OddSqrt ).

solve1( Input ) ->
    Num = erlang:list_to_integer( Input ),
    calc( Num ).


-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 0, solve1( "1" ) ),
      ?_assertEqual( 3, solve1( "12" ) ),
      ?_assertEqual( 2, solve1( "23" ) ),
      ?_assertEqual( 31, solve1( "1024" ) ) ].
