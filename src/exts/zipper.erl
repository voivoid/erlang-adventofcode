-module(zipper).
-export([make/1, next/1, next_n/2, get/1, update/2, to_list/1]).

-type zipper( T ) :: { list( T ), list( T ) }.

-export_type([zipper/1]).


-spec make( list( T ) ) -> zipper( T ).
make( L ) -> { [], L }.

-spec next( zipper( T ) ) -> zipper( T ).
next( { Prev, [] } ) ->
    { [], lists:reverse( Prev ) };
next( { Prev, [ Current | Next ] } ) ->
                   { [ Current | Prev ], Next }.

-spec next_n( zipper( T ), non_neg_integer() ) -> zipper( T ).
next_n( Z, N ) -> algos:iterate( fun next/1, Z, N ).
    

-spec get( zipper( T ) ) -> T.
get( { _, [ Current | _ ] } ) ->
    Current.

-spec update( T, zipper( T ) ) -> zipper( T ).
update( New, { Prev, [ _ | Next ] } ) ->
    { Prev, [ New | Next ] }.

-spec to_list( zipper( T ) ) -> list( T ).
to_list( { Prev, Next } ) ->
    lists:reverse( Prev ) ++ Next.
