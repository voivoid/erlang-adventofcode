-module(problem2015_16).
-export([solve1/1, solve2/1]).

%%% COMMON

-type compound() :: { atom(), non_neg_integer() }.
-type compounds() :: [ compound() ].
-type compound_checker() :: fun( ( compound() ) -> boolean() ).
-type aunt_num() :: non_neg_integer().
-type aunt() :: { aunt_num(), compounds() }.

-spec parse_compounds( [ string() ] ) -> compounds().
parse_compounds( [] ) -> [];
parse_compounds( [ Name, Qty | Rest ] ) ->
    Compound = { erlang:list_to_atom( Name ), erlang:list_to_integer( Qty ) },
    [ Compound | parse_compounds( Rest ) ].

-spec parse_aunt( string() ) -> aunt().
parse_aunt( Line ) ->
    [ _, AuntNum | Compounds ] = string:tokens( Line, " :," ),
    { erlang:list_to_integer( AuntNum ), parse_compounds( Compounds ) }.

-spec solve( string(), compound_checker() ) -> aunt_num().
solve( Input, CheckCompounds ) ->
    Lines = string:tokens( Input, "\n" ),
    Aunts = [ parse_aunt( Line ) || Line <- Lines ],
    { AuntNum, _ } = listz:find( fun( { _, Compounds } ) ->
                                         lists:all( CheckCompounds, Compounds )
                                 end,
                                 Aunts ),
    AuntNum.

%%% PART 1

-spec check_compound_1( compound() ) -> boolean().
check_compound_1( { children, 3 } ) -> true;
check_compound_1( { cats, 7 } ) -> true;
check_compound_1( { samoyeds, 2 } ) -> true;
check_compound_1( { pomeranians, 3 } ) -> true;
check_compound_1( { akitas, 0 } ) -> true;
check_compound_1( { vizslas, 0 } ) -> true;
check_compound_1( { goldfish, 5 } ) -> true;
check_compound_1( { trees, 3 } ) -> true;
check_compound_1( { cars, 2 } ) -> true;
check_compound_1( { perfumes, 1 } ) -> true;
check_compound_1( _ ) -> false.

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve( Input, fun check_compound_1/1 ).

%%% PART 2

-spec check_compound_2( compound() ) -> boolean().
check_compound_2( { cats, N } ) -> N > 7;
check_compound_2( { trees, N } ) -> N > 3;
check_compound_2( { pomeranians, N } ) -> N < 3;
check_compound_2( { goldfish, N } ) -> N < 5;
check_compound_2( Compound ) -> check_compound_1( Compound ).

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    solve( Input, fun check_compound_2/1 ).
