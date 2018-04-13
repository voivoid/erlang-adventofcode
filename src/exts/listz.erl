-module(listz).
-export([index/2, shiftl/2, shiftr/2, find/2]).

-spec index( T, list( T ) ) -> non_neg_integer() | not_found.
index( X, List ) -> index_impl( X, List, 1 ).

-spec index_impl( T, list( T ), non_neg_integer() ) -> non_neg_integer().
index_impl( _, [], _ ) -> not_found;
index_impl( X, [ X | _ ], Counter ) -> Counter;
index_impl( X, [ _ | XS ], Counter ) -> index_impl( X, XS, Counter + 1 ).


-spec shiftl( non_neg_integer(), list( T ) ) -> list( T ).
shiftl( _, [] ) -> [];
shiftl( N, L ) ->  shiftl_impl( N rem erlang:length( L ) , L, [] ).

-spec shiftl_impl( non_neg_integer(), list( T ), list( T ) ) -> list( T ).
shiftl_impl( 0, XS, Acc ) -> XS ++ lists:reverse( Acc );
shiftl_impl( N, [ X | XS ], Acc ) -> shiftl_impl( N - 1, XS, [ X | Acc ] ).
    

-spec shiftr( non_neg_integer(), list( T ) ) -> list( T ).
shiftr( N, L ) -> shiftl( erlang:length( L ) - N, L ).

-spec find( fun( ( T ) -> boolean() ), list( T ) ) -> T | not_found.
find( _, [] ) -> not_found;
find( Pred, [ X | XS ] ) -> case Pred( X ) of
                                false -> find( Pred, XS );
                                true -> X
                            end.
