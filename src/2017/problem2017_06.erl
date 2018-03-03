-module(problem2017_06).
-export([solve1/1, solve2/1]).

-type block() :: non_neg_integer().
-type banks() :: [ block() ].
-type history() :: sets:set( banks() ).


-spec reallocate( banks() ) -> banks().
reallocate( Banks ) ->
    NumOfBanks = erlang:length( Banks ),
    MaxBank = lists:max( Banks ),
    MaxBankIdx = lists_exts:index( MaxBank, Banks ),
    IncEveryBankBy = MaxBank div NumOfBanks,
    NumOfBanksToInc = MaxBank rem NumOfBanks,

    BankIncs = fun() -> 
                       BankIncs = lists:append( [ [-MaxBank], lists:duplicate( NumOfBanksToInc, 1 ), lists:duplicate( NumOfBanks - 1 - NumOfBanksToInc, 0 ) ] ),
                       ShiftedBankIncs = lists_exts:shiftr( MaxBankIdx - 1, BankIncs ),
                       lists:map( fun( N ) -> N + IncEveryBankBy end, ShiftedBankIncs ) end (),
    
    
    lists:zipwith( fun erlang:'+'/2, Banks, BankIncs ).

-spec run_reallocations( banks(), history(), non_neg_integer() ) -> { non_neg_integer(), banks() }.
run_reallocations( Banks, History, Counter ) ->
    AlreadyExists = sets:is_element( Banks, History ),
    if AlreadyExists -> { Counter, Banks };
       not AlreadyExists ->
            run_reallocations( reallocate( Banks ), sets:add_element( Banks, History ), Counter + 1 )
    end.

-spec get_banks( string() ) -> [ non_neg_integer() ].
get_banks( Input ) ->
    lists:map( fun erlang:list_to_integer/1, string:tokens( Input, " \t" ) ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Banks = get_banks( Input ),
    { Counter, _ } = run_reallocations( Banks, sets:new(), 0 ),
    Counter.
    

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    Banks = get_banks( Input ),
    { _, LastBanks } = run_reallocations( Banks, sets:new(), 0 ),
    { Counter, _ } = run_reallocations( LastBanks, sets:new(), 0 ),
    Counter.
