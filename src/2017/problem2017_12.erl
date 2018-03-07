-module(problem2017_12).
-export([solve1/1]).

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

    lists:foldl( fun( ConnID, CountedAcc ) -> 
                         sets:add_element( ConnID, count_connections_with( ConnID, DataMap, CountedAcc ) ) end,
                 Counted1,
                 sets:to_list( UniqueConns ) ).
    

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    DataMap = parse_input( Input ),
    sets:size( count_connections_with( 0, DataMap, sets:from_list( [ 0 ]) ) ).


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
