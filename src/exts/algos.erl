-module(algos).
-export([iterate/3]).

-spec iterate( fun( ( T ) -> T ), T, non_neg_integer() ) -> T.
iterate( _, Elem, 0 ) ->
    Elem;
iterate( F, Elem, Counter ) ->
    iterate( F, F( Elem ), Counter - 1 ).

