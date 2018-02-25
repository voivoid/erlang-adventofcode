-module(problem2017_05).
-export([solve1/1, solve2/1]).


-type offset_calculator() :: fun( ( integer() ) -> integer() ).

-spec jump( Offsets :: array:array( integer() ),
            Len :: integer(),
            Idx :: array:array_index(),
            Counter :: integer(),
            CalcNewOffset :: offset_calculator() )
          -> non_neg_integer().

jump( _, Len, Idx, Counter, _CalcNewOffset ) when Idx < 0; Idx >= Len -> Counter;
jump( Offsets, Len, Idx, Counter, CalcNewOffset ) ->
    Offset = array:get( Idx, Offsets ),
    NewOffset = CalcNewOffset( Offset ),
    UpdatedOffsets = array:set( Idx, NewOffset, Offsets ),
    jump( UpdatedOffsets , Len, Idx + Offset, Counter + 1, CalcNewOffset ).

-spec solve( string(), offset_calculator() ) -> non_neg_integer().
solve( Input, CalcNewOffset ) ->
    Offsets = array:from_list( lists:map( fun list_to_integer/1, string:tokens( Input, "\n" ) ) ),
    jump( Offsets, array:size( Offsets ), 0, 0, CalcNewOffset ).

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


