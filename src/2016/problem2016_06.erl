-module(problem2016_06).
-export([solve1/1, solve2/1]).


-type freq_map() :: #{ char() := non_neg_integer() }.
-type freq_list() :: list( freq_map() ).
-type algo() :: most_common | least_common.

-spec zip_char_with_freqs( char(), freq_map() ) -> freq_map().
zip_char_with_freqs( Char, FreqMap ) ->
    maps:update_with( Char, fun( C ) -> C + 1 end, 1, FreqMap ).

-spec calc_characters_frequencies( list( string() ) ) -> freq_list().
calc_characters_frequencies( [ L | _ ] = Lines ) ->
    LineLen = erlang:length( L ),

    lists:foldl( fun( Line, FreqsList ) ->
                         lists:zipwith( fun zip_char_with_freqs/2,
                                        Line,
                                        FreqsList )
                 end,
                 lists:duplicate( LineLen, #{} ),
                 Lines ).

-spec find_most_freq_char( freq_map(), algo() ) -> char().
find_most_freq_char( FreqsMap, CorrectionAlgo ) ->
    CharsFreqs = maps:to_list( FreqsMap ),
    SortedFreqs = lists:keysort( 2, CharsFreqs ),

    { MostCommonChar, _ } =
        case CorrectionAlgo of
            most_common -> lists:last( SortedFreqs );
            least_common -> lists:nth( 1, SortedFreqs )
        end,

    MostCommonChar.

-spec calc_most_common_chars( freq_list(), algo() ) -> string().
calc_most_common_chars( FreqsList, CorrectionAlgo ) ->
    lists:map( fun( FreqsMap ) -> find_most_freq_char( FreqsMap, CorrectionAlgo ) end, FreqsList ).

-spec solve( string() , algo() ) -> string().
solve( Input, CorrectionAlgo ) ->
    Lines = string:tokens( Input, "\n " ),
    FreqsList = calc_characters_frequencies( Lines ),
    calc_most_common_chars( FreqsList, CorrectionAlgo ).

-spec solve1( string() ) -> string().
solve1( Input ) ->
    solve( Input, most_common ).

-spec solve2( string() ) -> string().
solve2( Input ) ->
    solve( Input, least_common ).

-include_lib("eunit/include/eunit.hrl").

test_input() -> "
        eedadn
        drvtee
        eandsr
        raavrd
        atevrs
        tsrnev
        sdttsa
        rasrtv
        nssdts
        ntnada
        svetve
        tesnvt
        vntsnd
        vrdear
        dvrsen
        enarar
    ".

solve1_test_() ->
    [ ?_assertEqual( "easter", solve1( test_input() ) ) ].

solve2_test_() ->
    [ ?_assertEqual( "advent", solve2( test_input() ) ) ].
