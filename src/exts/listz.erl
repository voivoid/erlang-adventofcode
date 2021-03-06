-module(listz).
-export([index/2, shiftl/2, shiftr/2, find/2, iterate/3, foldl_stoppable/4, permutations/1, set_elem/3, insert/3, replace/3]).

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

-spec set_elem( T, non_neg_integer(), [ T ] ) -> [ T ].
set_elem( Val, 1, [ _ | XS ] ) -> [ Val | XS ];
set_elem( Val, Idx, [ X | XS ] ) -> [ X | set_elem( Val, Idx - 1, XS ) ];
set_elem( _, _, _ ) -> error( badarg ).

-spec insert( [ T ], non_neg_integer(), [ T ] ) -> [ T ].
insert( InsertedList, 0, List ) -> lists:append( InsertedList, List );
insert( InsertedList, Idx, [ X | XS ] ) -> [ X | insert( InsertedList, Idx - 1, XS ) ];
insert( _, _, _ ) -> error( badarg ).

-spec replace( [ T ], non_neg_integer(), [ T ] ) -> [ T ].
replace( ReplacementList, 1, [ _ | XS ] ) -> lists:append( ReplacementList, XS );
replace( ReplacementList, Idx, [ X | XS ] ) -> [ X | replace( ReplacementList, Idx - 1, XS ) ];
replace( _, _, _ ) -> error( badarg ).
