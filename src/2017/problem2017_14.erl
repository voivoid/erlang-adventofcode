-module(problem2017_14).
-export([solve1/1, solve2/1, calc_hash/1]).

-type hash() :: nonempty_string().
-type bit() :: 0 | 1.
-type bitlist() :: [ bit() ].
-type bitset() :: sets:set( bit() ).
-type coord() :: { non_neg_integer(), non_neg_integer() }.
-type coords() :: [ coord() ].
-type visited() :: sets:set( coord() ).

-spec gen_inputs( string() ) -> [ string() ].
gen_inputs( Input ) ->
    lists:map( fun( N ) -> lists:concat( [ Input, "-", N ] ) end, lists:seq( 0, 127 ) ).

-spec calc_hash( string() ) -> hash().
calc_hash( Str ) ->
    problem2017_10:solve2( Str ).

-spec translate_to_bits( hash() ) -> bitlist().
translate_to_bits( Hash ) ->
    Bin = << << ( erlang:list_to_integer( [ HexChar ], 16 ) ):4 >> || HexChar <- Hash >>,
    [ Bit || <<Bit:1>> <= Bin ].

-spec get_bits_2d_list( string() ) -> [ bitlist() ].
get_bits_2d_list( Input ) ->
    Inputs = gen_inputs( Input ),
    Hashes = rpc:pmap({?MODULE, calc_hash}, [], Inputs ),
    lists:map( fun translate_to_bits/1, Hashes ).


-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Bits2dList = get_bits_2d_list( Input ),
    lists:sum( lists:map( fun lists:sum/1, Bits2dList ) ).

-spec make_bitset( bitlist() ) -> bitset().
make_bitset( Bits2dList ) ->
    ListLen = 128,
    ListLen = erlang:length( Bits2dList ),
    lists:foldl( fun( { Bits, Y }, BitSet ) ->
                         ListLen = erlang:length( Bits ),
                         lists:foldl( fun( { 0, _ }, SetAcc ) -> SetAcc;
                                         ( { 1, X }, SetAcc ) -> sets:add_element( { X, Y }, SetAcc )
                                      end,
                                      BitSet,
                                      lists:zip( Bits, lists:seq( 1, ListLen ) ) )
                 end,
                 sets:new(),
                 lists:zip( Bits2dList, lists:seq( 1, ListLen ) ) ).

-spec neighbours_coords( coord() | non_neg_integer() ) -> list( non_neg_integer() ).
neighbours_coords( { X, Y } ) ->
    [ { X1, Y } || X1 <- neighbours_coords( X ) ] ++
    [ { X, Y1 } || Y1 <- neighbours_coords( Y ) ];
neighbours_coords( 1 ) -> [ 2 ];
neighbours_coords( 128 ) -> [ 127 ];
neighbours_coords( N ) -> [ N - 1, N + 1 ].

-spec get_neighbours( coord(), bitset(), visited() ) -> { coords(), visited() }.
get_neighbours( Coord, BitSet, Visited ) ->
    IsVisited = sets:is_element( Coord, Visited ),
    HasBit = ( not IsVisited ) andalso sets:is_element( Coord, BitSet ),

    VisitedWithCoord = sets:add_element( Coord, Visited ),
    case HasBit of
        false -> { [], VisitedWithCoord } ;
        _ ->
            lists:foldl( fun( NeighbourCoord, { NeighboursAcc, VisitedAcc } ) ->
                                    { NewNeighbours, NewVisited } = get_neighbours( NeighbourCoord, BitSet, VisitedAcc ),
                                    { lists:merge( NewNeighbours, NeighboursAcc ), sets:union( NewVisited, VisitedAcc ) }
                         end,
                         { [ Coord ], VisitedWithCoord },
                         neighbours_coords( Coord )
                       )
    end.

-spec get_neighbours( coord(), bitset() ) -> coords().
get_neighbours( Coord, BitSet ) ->
    Visited = sets:new(),
    { Neighbours, _ } = get_neighbours( Coord, BitSet, Visited ),
    Neighbours.

-spec count_regions( bitset() ) -> non_neg_integer().
count_regions( BitSet ) ->
    case sets:to_list( BitSet ) of
        [] -> 0;
        [ BitCoord | _ ] ->
            Neighbours = get_neighbours( BitCoord, BitSet ),
            BitSetWithRemovedNeighbours = lists:foldl( fun( Neighbour, SetAcc ) ->
                                                               sets:del_element( Neighbour, SetAcc )
                                                       end,
                                                       BitSet,
                                                       Neighbours),
            1 + count_regions( BitSetWithRemovedNeighbours )
    end.


-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    Bits2dList = get_bits_2d_list( Input ),
    BitSet = make_bitset( Bits2dList ),
    count_regions( BitSet ).



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 8108, solve1( "flqrgnkx" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 1242, solve2( "flqrgnkx" ) ) ].
