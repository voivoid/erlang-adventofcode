-module(problem2017_17).
-export([solve1/1, solve2/1]).

run_steps( _, Zipper, Counter, Counter ) ->
    Zipper;
run_steps( Stepping, Zipper, StopN, Counter ) ->
    UpdatedZipper = zipper:prepend( Counter, zipper:next_n( Stepping + 1, Zipper ) ),
    run_steps( Stepping, UpdatedZipper, StopN, Counter + 1 ).    

solve1( Input ) ->
    Stepping = erlang:list_to_integer( Input ),
    Zipper = run_steps( Stepping, zipper:from_list( [0] ), 2018, 1 ),
    zipper:get( zipper:next( Zipper ) ).

find_next_after_zero( Zipper ) ->
    case zipper:get( Zipper ) of
        0 -> zipper:get( zipper:next( Zipper ) );
        _ -> find_next_after_zero( zipper:next( Zipper ) )
    end.
        
solve2( Input ) ->
    Stepping = erlang:list_to_integer( Input ),
    Zipper = run_steps( Stepping, zipper:from_list( [0] ), 50000001, 1 ),
    find_next_after_zero( Zipper ).

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 638, solve1( "3" ) ) ].
