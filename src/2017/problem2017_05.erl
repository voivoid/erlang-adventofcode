-module(problem2017_05).
-export([solve1/1, solve2/1]).


-type offset_calculator() :: fun( ( integer() ) -> integer() ).

-spec jump( Offsets :: #{ integer() := integer() },
            Len :: integer(),
            Idx :: integer(),
            Counter :: integer(),
            CalcNewOffset :: offset_calculator() )
          -> non_neg_integer().

jump( _, Len, Idx, Counter, _ ) when Idx < 0; Idx >= Len -> Counter;
jump( OffsetsMap, Len, Idx, Counter, CalcNewOffset ) ->
    Offset = maps:get( Idx, OffsetsMap ),
    NewOffset = CalcNewOffset( Offset ),
    jump( maps:put( Idx, NewOffset, OffsetsMap ),Len, Idx + Offset, Counter + 1, CalcNewOffset ).

-spec solve( string(), offset_calculator() ) -> non_neg_integer().
solve( Input, CalcNewOffset ) ->
    Offsets = lists:map( fun list_to_integer/1, string:tokens( Input, "\n" ) ),
    OffsetsMap = maps:from_list( lists:zip( lists:seq( 0, erlang:length( Offsets ) - 1 ), Offsets ) ),
    jump( OffsetsMap, erlang:length( Offsets ), 0, 0, CalcNewOffset ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    CalcNewOffset = fun( Offset ) -> Offset + 1 end,
    solve( Input, CalcNewOffset ).

-spec solve2( string() ) -> non_neg_integer().
solve2(Input) ->
    CalcNewOffset = fun( Offset ) ->
                            if Offset >= 3 -> Offset - 1;
                               Offset < 3  -> Offset + 1
                            end
                    end,
    solve( Input, CalcNewOffset ).




-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 5 , solve1( "0\n3\n0\n1\n-3" ) ) ].


solve2_test_() ->
    [ ?_assertEqual( 10, solve2( "0\n3\n0\n1\n-3" ) ) ].
