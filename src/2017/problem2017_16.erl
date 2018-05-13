-module(problem2017_16).
-export([solve1/1, solve2/1]).

-type index() :: non_neg_integer().
-type prog() :: char().
-type progs() :: [ prog() ].
-type move() :: { s, non_neg_integer() } | { x, index(), index() } | { p, prog(), prog() }.
-type moves() :: [ move() ].


-spec do_move( move(), progs() ) -> progs().
do_move( { s, ProgsToMove }, Progs ) ->
    listz:shiftr( ProgsToMove, Progs );
do_move( { x, From, To }, Progs ) ->
    A = lists:nth( From + 1, Progs ),
    B = lists:nth( To + 1, Progs ),
    do_move( { p, A, B }, Progs );
do_move( { p, A, B }, Progs ) ->
    lists:map( fun( C ) when C == A -> B;
                  ( C ) when C == B -> A;
                  ( C ) -> C
               end,
               Progs ).

-spec run_dance( moves(), progs()  ) -> progs().
run_dance( Moves, Progs ) ->
    lists:foldl( fun do_move/2,
                 Progs,
                 Moves ).

-spec parse_move( string() ) -> move().
parse_move( [ $s | XS ] ) ->
    { s, erlang:list_to_integer( XS ) };
parse_move( [ $x | Indexes ] ) ->
    [ Idx1, Idx2 ] = string:tokens( Indexes, "/" ),
    { x, erlang:list_to_integer( Idx1 ), erlang:list_to_integer( Idx2 ) };
parse_move( [ $p, A, $/, B ] ) ->
    { p, A, B }.

-spec parse_moves( string() ) -> moves().
parse_moves( Input ) ->
    lists:map( fun parse_move/1, string:tokens( Input, "," ) ).

-spec get_progs() -> progs().
get_progs() ->
    lists:seq( $a, $p ).

-spec solve1( string() ) -> string().
solve1( Input ) ->
    solve1( Input, get_progs() ).

-spec solve1( string(), string() ) -> string().
solve1( Input, Progs ) ->
    run_dance( parse_moves( Input ) , Progs ).

-spec run_dances( moves(), progs(), progs(),  non_neg_integer(), non_neg_integer() ) -> progs().
run_dances( _, Progs, _, Counter, Counter ) -> Progs;
run_dances( Moves, Progs, Progs, Counter, DancesNum ) when Counter > 0 ->
    NextCounter = DancesNum - ( DancesNum rem Counter ),
    run_dances( Moves, Progs, [], NextCounter, DancesNum );
run_dances( Moves, Progs, Init, Counter, DancesNum ) ->
    run_dances( Moves, run_dance( Moves, Progs ), Init, Counter + 1, DancesNum ).

-spec solve2( string() ) -> string().
solve2( Input ) ->
    run_dances( parse_moves( Input ), get_progs(), get_progs(), 0, 1000000000 ).



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( "baedc", solve1( "s1,x3/4,pe/b", "abcde" ) ) ].
