-module(zipper).
-export([from_list/1, to_list/1, next/1, next_n/2, prev/1, prev_n/2, get/1, get_n/2, update/2, update_with_list/2, prepend/2, append/2, is_empty/1, is_first/1, is_last/1, pos/1, len/1]).

-type zipper( T ) :: { [T], [T] }.

-export_type([zipper/1]).


-spec from_list( [T] ) -> zipper( T ).
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

-spec get_n( non_neg_integer(), zipper( T ) ) -> [T].
get_n( N, Zipper ) -> get_n_impl( N, Zipper ).

-spec get_n_impl( non_neg_integer(), zipper( T ) ) -> [T].
get_n_impl( 0, _ ) -> [];
get_n_impl( N, Zipper ) ->
    [ zipper:get( Zipper ) | get_n_impl( N - 1, next( Zipper ) ) ].

-spec update( T, zipper( T ) ) -> zipper( T ).
update( New, { Prev, [ _ | Next ] } ) ->
    { Prev, [ New | Next ] }.

-spec update_with_list( [T], zipper( T ) ) -> zipper( T ).
update_with_list( Updates, Zipper ) ->
    lists:foldl( fun ( U, Z ) -> next( update( U, Z ) ) end, Zipper, Updates ).


-spec prepend( T, zipper( T ) ) -> zipper( T ).
prepend( New, { Prev, [ Current | Next ] } ) ->
    { Prev, [ New , Current | Next ] }.

-spec append( T, zipper( T ) ) -> zipper( T ).
append( New, { Prev, [ Current | Next ] } ) ->
    { Prev, [ Current, New | Next ] }.

-spec to_list( zipper( T ) ) -> [T].
to_list( { Prev, Next } ) ->
    lists:reverse( Prev ) ++ Next.

-spec is_empty( zipper( _ ) ) -> boolean().
is_empty( { [], [] } ) -> true;
is_empty( _ ) -> false.

-spec is_first( zipper( _ ) ) -> boolean().
is_first( { [], _ } ) -> true;
is_first( _ ) -> false.

-spec is_last( zipper( _ ) ) -> boolean().
is_last( { _, [] } ) -> true;
is_last( { _, [ _ ] } ) -> true;
is_last( _ ) -> false.

-spec pos( zipper( _ ) ) -> non_neg_integer().
pos( { Prev, _ } ) -> erlang:length( Prev ) + 1.

-spec len( zipper( _ ) ) -> non_neg_integer().
len( { Prev, Next } ) -> erlang:length( Prev ) + erlang:length( Next ).
