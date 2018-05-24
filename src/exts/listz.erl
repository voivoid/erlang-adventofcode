-module(listz).
-export([index/2, shiftl/2, shiftr/2, find/2, iterate/3, foldl_stoppable/4, permutations/1]).

-spec index( T, [T] ) -> non_neg_integer() | not_found.
index( X, List ) -> index_impl( X, List, 1 ).

-spec index_impl( T, [T], non_neg_integer() ) -> non_neg_integer().
index_impl( _, [], _ ) -> not_found;
index_impl( X, [ X | _ ], Counter ) -> Counter;
index_impl( X, [ _ | XS ], Counter ) -> index_impl( X, XS, Counter + 1 ).


-spec shiftl( non_neg_integer(), [T] ) -> [T].
shiftl( _, [] ) -> [];
shiftl( N, L ) ->  shiftl_impl( N rem erlang:length( L ) , L, [] ).

-spec shiftl_impl( non_neg_integer(), [T], [T] ) -> [T].
shiftl_impl( 0, XS, Acc ) -> XS ++ lists:reverse( Acc );
shiftl_impl( N, [ X | XS ], Acc ) -> shiftl_impl( N - 1, XS, [ X | Acc ] ).


-spec shiftr( non_neg_integer(), [T] ) -> [T].
shiftr( N, L ) -> shiftl( erlang:length( L ) - N, L ).

-spec find( fun( ( T ) -> boolean() ), [T] ) -> T | not_found.
find( _, [] ) -> not_found;
find( Pred, [ X | XS ] ) -> case Pred( X ) of
                                false -> find( Pred, XS );
                                true -> X
                            end.


-spec iterate( fun( ( T ) -> T ), T, non_neg_integer() ) -> [T].
iterate( _, _, 0 ) -> [];
iterate( F, Init, N ) ->
    lists:reverse( iterate_impl( F, Init, N - 1, [ Init ] ) ).

-spec iterate_impl( fun( ( T ) -> T ), T, non_neg_integer(), [T] ) -> [T].
iterate_impl( _, _, 0, Result ) -> Result;
iterate_impl( F, Init, N, Result ) ->
    NewVal = F( Init ),
    iterate_impl( F, NewVal, N - 1, [ NewVal | Result ] ).

-spec foldl_stoppable( fun( ( T, A ) -> A ), A, atom(), [T] ) -> A.
foldl_stoppable( _, Init, _, [] ) -> Init;
foldl_stoppable( F, Init, StopAtom, [ X | XS ] ) ->
    case F( X, Init ) of
        { StopAtom, Result } -> Result;
        Result -> foldl_stoppable( F, Result, StopAtom, XS )
    end.

-spec permutations( [T] ) -> [ [T] ].
permutations( [] ) -> [ [] ];
permutations( L ) -> [ [ X | XS ]  || X <- L, XS <- permutations( L -- [ X ] ) ].
