-module(problem2017_12).
-export([solve1/1, solve2/1]).

-type id() :: non_neg_integer().
-type connections() :: sets:set( id() ).
-type pipe() :: { id(), connections() }.
-type datamap() :: #{ id() := connections() }.

-spec parse_line( string() ) -> pipe().
parse_line( Line ) ->
    [ ID | Connections ] = string:tokens( Line, " <->," ),
    { erlang:list_to_integer( ID ), sets:from_list( lists:map( fun erlang:list_to_integer/1, Connections ) ) }.


-spec update_data_map( pipe(), datamap() ) -> datamap().
update_data_map( { ID, Connections }, Map ) ->
    maps:update_with( ID, fun( ExistingConns ) -> sets:union( ExistingConns, Connections ) end, Connections, Map ).

-spec parse_input( string() ) -> datamap().
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:foldl( fun( Line, Map ) ->
                         update_data_map( parse_line( Line ), Map )
                 end,
                 #{},
                 Lines ).

-spec count_connections_with( id(), datamap(), connections() ) -> connections().
count_connections_with( ID, DataMap, Counted ) ->
    Connections = maps:get( ID, DataMap, sets:new() ),
    Counted1 = sets:add_element( ID, Counted ),
    UniqueConns = sets:subtract( Connections, Counted1 ),

    sets:fold( fun( UniqueConnID, CountedAcc ) -> 
                       count_connections_with( UniqueConnID, DataMap, CountedAcc ) end,
               Counted1,
               UniqueConns ).

-spec count_connection_groups( datamap() ) -> non_neg_integer().
count_connection_groups( DataMap ) ->
    case maps:keys( DataMap ) of
        [] -> 0;
        [ Key | _ ] ->
            Group = count_connections_with( Key, DataMap, sets:new() ),
            FilteredMap = maps:filter( fun( K, _ ) -> not sets:is_element( K, Group ) end, DataMap ),
            1 + count_connection_groups( FilteredMap )
    end.

    

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    DataMap = parse_input( Input ),
    sets:size( count_connections_with( 0, DataMap, sets:new() ) ).                                            

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    DataMap = parse_input( Input ),
    count_connection_groups( DataMap ).
    

-include_lib("eunit/include/eunit.hrl").

test_input() ->
    "0 <-> 2
     1 <-> 1
     2 <-> 0, 3, 4
     3 <-> 2, 4
     4 <-> 2, 3, 6
     5 <-> 6
     6 <-> 4, 5".

solve1_test_() ->
    [ ?_assertEqual( 6, solve1( test_input() ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 2, solve2( test_input() ) ) ].
