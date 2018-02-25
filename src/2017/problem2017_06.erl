-module(problem2017_06).
-export([solve1/1]).

-type block() :: non_neg_integer().
-type banks() :: [ block() ].
-type history() :: sets:set( banks() ).

-spec find_max_bank( banks() ) -> { non_neg_integer(), block() }
find_max_bank( Banks ) ->
    0.

-spec reallocate( banks() ) -> banks().
reallocate( Banks ) ->
    Banks.

-spec iterate( banks(), history(), non_neg_integer() ) -> non_neg_integer().
iterate( Banks, History, Counter ) ->
    AlreadyExists = sets:is_element( Banks, History ),
    if AlreadyExists -> Counter;
       not AlreadyExists ->
            iterate( reallocate( Banks ), sets:add_element( Banks, History ), Counter + 1 )
    end.
            

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Banks = lists:map( fun erlang:list_to_integer/1, string:split( Input, " \t" ) ),
    iterate( Banks, sets:new(), 0 ).
    
