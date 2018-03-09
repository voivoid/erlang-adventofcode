-module(lists_exts).
-export([index/2, shiftl/2, shiftr/2, parallel_map/2]).

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


-spec parallel_map( fun( ( T ) -> T ), list( T ) ) -> list( T ).
parallel_map( F, XS ) ->
    Parent = self(),
    Running = [ erlang:spawn_monitor( fun() -> Parent ! { self(), F( X ) } end ) || X <- XS ],
    parallel_map_collect( Running ).

-spec parallel_map_collect( list( { pid(), reference() } ) ) -> list().
parallel_map_collect( [] ) -> [];
parallel_map_collect( [ { Pid, Ref } | XS ] ) -> 
    receive
        { Pid, Res } ->
            erlang:demonitor( Ref, [flush] ),
            [ Res | parallel_map_collect( XS ) ]
    after 10000 ->
            error( parallel_map_timeout )
    end.
              
        
            
    
    
