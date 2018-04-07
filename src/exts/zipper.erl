-module(zipper).
-export([from_list/1, next/1, next_n/2, prev/1, prev_n/2, get/1, get_n/2, update/2, update_with_list/2, to_list/1, prepend/2, append/2]).

-type zipper( T ) :: { list( T ), list( T ) }.

-export_type([zipper/1]).


-spec from_list( list( T ) ) -> zipper( T ).
from_list( L ) -> { [], L }.

-spec next( zipper( T ) ) -> zipper( T ).
next( { Prev, [ Current ] } ) ->
    { [], lists:reverse( [ Current | Prev ] ) };
next( { Prev, [ Current | Next ] } ) ->
                   { [ Current | Prev ], Next }.

-spec prev( zipper( T ) ) -> zipper( T ).
prev( { [], Next } ) ->
    [ Last | First ] = lists:reverse( Next ),
    { First, [ Last ] };
prev( { [ Prev | Prevs ], Next } ) ->
    { Prevs, [ Prev | Next ] }.

-spec prev_n( integer(), zipper( T ) ) -> zipper( T ).
prev_n( N, Zipper ) when N >= 0 -> algos:iterate( fun prev/1, Zipper, N );
prev_n( N, Zipper ) when N < 0 -> next_n( erlang:abs( N ), Zipper ).

-spec next_n( integer(), zipper( T ) ) -> zipper( T ).
next_n( N, Zipper ) when N >= 0 -> algos:iterate( fun next/1, Zipper, N );
next_n( N, Zipper ) when N < 0 -> prev_n( erlang:abs( N ), Zipper ).
    

-spec get( zipper( T ) ) -> T.
get( { _, [ Current | _ ] } ) ->
    Current.

-spec get_n( non_neg_integer(), zipper( T ) ) -> list( T ).
get_n( N, Zipper ) -> get_n_impl( N, Zipper ).

-spec get_n_impl( non_neg_integer(), zipper( T ) ) -> list( T ).
get_n_impl( 0, _ ) -> [];
get_n_impl( N, Zipper ) ->
    [ zipper:get( Zipper ) | get_n_impl( N - 1, next( Zipper ) ) ].

-spec update( T, zipper( T ) ) -> zipper( T ).
update( New, { Prev, [ _ | Next ] } ) ->
    { Prev, [ New | Next ] }.

-spec update_with_list( list( T ), zipper( T ) ) -> zipper( T ).
update_with_list( Updates, Zipper ) ->
    lists:foldl( fun ( U, Z ) -> next( update( U, Z ) ) end, Zipper, Updates ).


-spec prepend( T, zipper( T ) ) -> zipper( T ).
prepend( New, { Prev, [ Current | Next ] } ) ->
    { Prev, [ New , Current | Next ] }.

-spec append( T, zipper( T ) ) -> zipper( T ).
append( New, { Prev, [ Current | Next ] } ) ->
    { Prev, [ Current, New | Next ] }.

-spec to_list( zipper( T ) ) -> list( T ).
to_list( { Prev, Next } ) ->
    lists:reverse( Prev ) ++ Next.
