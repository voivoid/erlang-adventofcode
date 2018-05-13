-module(problem2016_09).
-export([solve1/1, solve2/1]).

%%% COMMON

-spec parse_marker( string() ) -> { non_neg_integer(), non_neg_integer() }.
parse_marker( Marker ) ->
    [ Chars, ToRepeat ] = string:tokens( Marker, "x" ),
    { erlang:list_to_integer( Chars ), erlang:list_to_integer( ToRepeat ) }.

-spec decompress( string(), boolean() ) -> non_neg_integer().
decompress( Line, RecursiveDescompression ) ->
    { Left, MarkerPlusRest }  = lists:splitwith( fun( C ) -> C /= $( end, Line ),
    LeftLen = erlang:length( Left ),
    case MarkerPlusRest of
        [] -> LeftLen;
        _ ->
            { [ $( | Marker ], [ $) | Rest ] } = lists:splitwith( fun( C ) -> C /= $) end, MarkerPlusRest ),
            { CharsToTake, TimesToRepeat } = parse_marker( Marker ),

            { CharsToRepeat, Right } = lists:split( CharsToTake, Rest ),

            CharsToRepeatLen =
                case RecursiveDescompression of
                    true -> decompress( CharsToRepeat, RecursiveDescompression );
                    false -> CharsToTake
                end,

            LeftLen + ( TimesToRepeat * CharsToRepeatLen ) + decompress( Right, RecursiveDescompression )
    end.


%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    decompress( Input, false ).

%%% PART 2

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    decompress( Input, true ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [
     ?_assertEqual(  6, solve1( "ADVENT" ) ),
     ?_assertEqual(  7, solve1( "A(1x5)BC" ) ),
     ?_assertEqual(  9, solve1( "(3x3)XYZ" ) ),
     ?_assertEqual( 11, solve1( "A(2x2)BCD(2x2)EFG" ) ),
     ?_assertEqual(  6, solve1( "(6x1)(1x3)A" ) ),
     ?_assertEqual( 18, solve1( "X(8x2)(3x3)ABCY" ) )
    ].

solve2_test_() ->
    [
     ?_assertEqual(  9, solve2( "(3x3)XYZ" ) ),
     ?_assertEqual( 20, solve2( "X(8x2)(3x3)ABCY" ) ),
     ?_assertEqual( 241920, solve2( "(27x12)(20x12)(13x14)(7x10)(1x12)A" ) ),
     ?_assertEqual( 445, solve2( "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" ) )
    ].
