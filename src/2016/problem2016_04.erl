-module(problem2016_04).
-compile([export_all, nowarn_export_all]).
-export([solve1/1]).

-record(room, { name :: string(), id :: integer(), checksum :: string() }).
-type room() :: #room{}.
-type rooms() :: [ room() ].

-type letter() :: $a..$z.
-type letters_freqs() :: orddict:orddict( letter(), non_neg_integer() ).

-spec parse_room( string() ) -> room().
parse_room( Line ) ->
    Tokens = string:tokens( Line, "-[]" ),
    { Name, [ Id, Checksum ] } = lists:split( erlang:length( Tokens ) - 2, Tokens ),
    #room{ name = string:join( Name, "-" ), id = erlang:list_to_integer( Id ), checksum = Checksum }.

-spec make_letter_frequency_dict( string() ) -> letters_freqs().
make_letter_frequency_dict( Letters ) ->
    LettersWithoutDashes = lists:filter( fun( $- ) -> false;
                                            ( _  ) -> true
                                         end,
                                         Letters ),
    lists:foldl( fun( Letter, Dict ) ->
                         orddict:update( Letter, fun( Counter ) -> Counter + 1 end, 1, Dict )
                 end,
                 orddict:new(),
                 LettersWithoutDashes ).

-spec get_sorted_by_frequency_letters_list( letters_freqs() ) -> [ { letter(), non_neg_integer() } ].
get_sorted_by_frequency_letters_list( LettersDict ) ->
    lists:reverse( lists:keysort( 2, lists:reverse( orddict:to_list( LettersDict ) ) ) ).

-spec is_real_room( room() ) -> boolean().
is_real_room( Room ) ->
    LettersFreqDict = make_letter_frequency_dict( Room#room.name ),
    LettersFreqList = get_sorted_by_frequency_letters_list( LettersFreqDict ),

    { Top5LettersAndFreqs, _ } = lists:split( 5, LettersFreqList ),
    { Top5Letters, _ } = lists:unzip( Top5LettersAndFreqs ),

    Top5Letters == Room#room.checksum.

-spec filter_decoys( rooms() ) -> rooms().
filter_decoys( Rooms ) ->
    lists:filter( fun is_real_room/1, Rooms ).

-spec get_real_rooms( string() ) -> rooms().
get_real_rooms( Input ) ->
    Lines = string:tokens( Input, "\n " ),
    Rooms = lists:map( fun parse_room/1, Lines ),
    filter_decoys( Rooms ).

-spec shift_letter( letter(), non_neg_integer() ) -> letter().
shift_letter( $-, _ ) -> $ ;
shift_letter( L, N ) ->
    LatinLetters = ( $z - $a ) + 1,
    Offset = N rem LatinLetters,
    $a + ( ( Offset + ( L - $a ) ) rem LatinLetters ).

-spec decrypt( room() ) -> string().
decrypt( Room ) ->
    lists:map( fun( Letter ) -> shift_letter( Letter, Room#room.id ) end, Room#room.name ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    RealRooms = get_real_rooms( Input ),
    lists:sum( lists:map( fun( Room ) -> Room#room.id end, RealRooms ) ).

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    RealRooms = get_real_rooms( Input ),
    DecryptedRooms = lists:map( fun( Room ) -> { Room, decrypt( Room ) } end, RealRooms ),

    TheRoomName = "northpole object storage",
    { TheRoom, _ } = listz:find( fun( { _, DecryptedName } ) -> DecryptedName == TheRoomName end, DecryptedRooms ),

    TheRoom#room.id.



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    Input = "aaaaa-bbb-z-y-x-123[abxyz]
             a-b-c-d-e-f-g-h-987[abcde]
             not-a-real-room-404[oarel]
             totally-real-room-200[decoy]",

    [ ?_assertEqual( 1514, solve1( Input ) ) ].

solve2_test_() ->
    [ ?_assertEqual( $b, shift_letter( $a, 1 ) ),
      ?_assertEqual( $a, shift_letter( $z, 1 ) ),
      ?_assertEqual( $c, shift_letter( $x, 5 ) ),
      ?_assertEqual( $z, shift_letter( $z, 0 ) ),
      ?_assertEqual( "very encrypted name", decrypt( parse_room( "qzmt-zixmtkozy-ivhz-343[zimth]" ) ) ) ].
