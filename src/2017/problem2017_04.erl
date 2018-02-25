-module(problem2017_04).
-export([solve1/1, solve2/1]).

-type line_filter() :: fun( ( [ string() ] ) -> boolean() ).

-spec solve( string(), line_filter()  ) -> non_neg_integer().
solve(Input, IsValidLine) ->
    Lines = string:tokens( Input, "\n" ),
    Tokens = lists:map( fun( Line ) -> string:tokens( Line, " " ) end, Lines ),
    length( lists:filter( IsValidLine, Tokens ) ).    

-spec solve1( string() ) -> non_neg_integer().
solve1(Input) ->
    IsValidLine = fun ( Tokens ) ->
        TokensSet = sets:from_list( Tokens ),
        length( Tokens ) == sets:size( TokensSet ) end,
    solve( Input, IsValidLine ).


-spec solve2( string() ) -> non_neg_integer().
solve2(Input) ->
    IsValidLine = fun( Tokens ) ->
        Sorted = lists:map( fun lists:sort/1, Tokens ),
        length( Sorted ) == length( lists:usort( Sorted ) ) end,
    solve( Input, IsValidLine ).
                           
