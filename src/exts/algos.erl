-module(algos).
-export([iterate/3, get_md5_str/1]).

-spec iterate( fun( ( T ) -> T ), T, non_neg_integer() ) -> T.
iterate( _, Elem, 0 ) ->
    Elem;
iterate( F, Elem, Counter ) ->
    iterate( F, F( Elem ), Counter - 1 ).

-spec get_md5_str( string() ) -> string().
get_md5_str( Str ) ->
    MD5 = erlang:md5( Str ),
    lists:append( [ integer_to_list( X, 16 ) || <<X:4>> <= MD5 ] ).
