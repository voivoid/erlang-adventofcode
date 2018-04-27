-module(problem2016_03).
-export([solve1/1, solve2/1]).

-type side() :: non_neg_integer().
-type triangle() :: { side(), side(), side() }.

-spec parse_triangle( string() ) -> triangle().
parse_triangle( Line ) ->
    [ A, B, C ] = lists:map( fun erlang:list_to_integer/1, string:tokens( Line, " " ) ),
    { A, B, C }.

-spec is_right_triangle( triangle() ) -> boolean().
is_right_triangle( { A, B, C } ) ->
    [ S1, S2, S3 ] = lists:sort( [ A, B, C ] ),
    S1 + S2 > S3.

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    Triangles = lists:map( fun parse_triangle/1, Lines ),
    RightTriangles = lists:filter( fun is_right_triangle/1, Triangles ),

    erlang:length( RightTriangles ).

-spec process_lines( list( string() ) ) -> non_neg_integer().
process_lines( [] ) -> 0;
process_lines( [ { A1, A2, A3 }, { B1, B2, B3 }, { C1, C2, C3 } | Lines ] ) ->
    Triangles = [ { A1, B1,C1 }, { A2, B2, C2 }, { A3, B3, C3 } ],
    RightTriangles = lists:filter( fun is_right_triangle/1, Triangles ),
    erlang:length( RightTriangles ) + process_lines( Lines ).



-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    process_lines( lists:map( fun parse_triangle/1, Lines ) ).



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 0, solve1( "5 10 25" ) ) ].

solve2_test_() ->
    Input = "101 301 501
             102 302 502
             103 303 503
             201 401 601
             202 402 602
             203 403 603",
    [ ?_assertEqual( 6, solve2( Input ) ) ].
