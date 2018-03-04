-module(problem2017_12).
-export([solve1/1]).

-type id() :: non_neg_integer().
-type connections() :: list( id() ).
-type pipe() :: { id(), connections() }.
-type datamap() :: #{ id() := connections() }.

-spec parse_line( string() ) -> pipe().
parse_line( Line ) ->
    [ ID | Connections ] = string:tokens( Line, " <->," ),
    { erlang:list_to_integer( ID ), list:map( fun erlang:list_to_integer/1, Connections ) }.


-spec update_data_map( pipe(), datamap() ) -> datamap().
update_data_map( { ID, Connections }, Map ) ->
    maps:update_with( ID, fun( ExistingConns ) -> lists:merge( ExistingConns, lists:sort( Connections ) ) end, [], Map ).

-spec parse_input( string() ) -> datamap().
parse_input( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:foldl( fun( Line, Map ) ->
                         update_data_map( parse_line( Line ), Map )
                 end,
                 #{},
                 Lines ).

-spec count_connections_with( id(), datamap() ) -> non_neg_integer().
count_connections_with( _ID, _DataMap ) ->
    0.

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    DataMap = parse_input( Input ),
    count_connections_with( 0, DataMap ).



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
