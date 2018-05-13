-module(problem2016_07).
-export([solve1/1, solve2/1]).

-type ip() :: string().
-type aba() :: { char(), char() }.
-type seq() :: string().
-type seqs() :: [ seq() ].
-type seq_type() :: hypernet | supernet.
-type typed_seq() :: { seq(), seq_type() }.

-spec has_abba( string() ) -> boolean().
has_abba( [ A, B, B, A | _    ] ) when A /= B -> true;
has_abba( [ _          | Rest ] ) -> has_abba( Rest );
has_abba( _                     ) -> false.

-spec has_aba( seqs() ) -> false | aba().
has_aba( [ A, B, A | Rest ] ) when A /= B -> { { A, B }, [ B, A | Rest ] } ;
has_aba( [ _       | Rest ] ) -> has_aba( Rest );
has_aba( _                  ) -> false.

-spec has_aba( aba(), seqs() ) -> boolean().
has_aba( { A, B }, [ B, A, B | _    ] ) -> true;
has_aba( { A, B }, [ _       | Rest ] ) -> has_aba( { A, B }, Rest );
has_aba( _,        _                  ) -> false.

-spec check_seqs_are_abba( [ typed_seq() ] ) -> boolean().
check_seqs_are_abba( TypedSeqs ) ->
    Status = lists:foldl(
          fun( _,                  stop   ) -> stop;
             ( { _,   supernet  }, true   ) -> true;
             ( { Seq, supernet  }, false  ) -> has_abba( Seq );
             ( { Seq, hypernet },  Status ) -> case has_abba( Seq ) of
                                                   true -> stop;
                                                   false -> Status
                                               end
          end,
          false,
          TypedSeqs ),

    case Status of
        stop -> false;
        Status -> Status
    end.

-spec check_seqs_have_aba( aba(), seq_type(), [ typed_seq() ] ) -> boolean().
check_seqs_have_aba( _,        _,    []                       ) -> false;
check_seqs_have_aba( { A, B }, Type, [ { _,   Type } | Rest ] ) -> check_seqs_have_aba( { A, B }, Type, Rest );
check_seqs_have_aba( { A, B }, Type, [ { Seq, _    } | Rest ] ) ->
    case has_aba( { A, B }, Seq ) of
        true -> true;
        false -> check_seqs_have_aba( { A, B }, Type, Rest )
    end.

-spec check_seqs_are_ssl( [ typed_seq() ] ) -> boolean().
check_seqs_are_ssl( []                            ) -> false;
check_seqs_are_ssl( [ { Seq, Type } | TypedSeqs ] ) ->
    IsSSL =
        case has_aba( Seq ) of
                false -> false;
                { ABA, Rest } -> check_seqs_have_aba( ABA, Type, TypedSeqs ) orelse check_seqs_are_ssl( [ { Rest, Type } | TypedSeqs ] )
        end,

    case IsSSL of
        true -> true;
        false -> check_seqs_are_ssl( TypedSeqs )
    end.

-spec get_typed_seqs( ip() ) -> [ typed_seq() ].
get_typed_seqs( IP ) ->
    Seqs = string:tokens( IP, "[]" ),
    SeqsNum = erlang:length( Seqs ),
    Types = listz:iterate( fun( hypernet ) -> supernet;
                              ( supernet ) -> hypernet
                           end,
                           supernet,
                           SeqsNum ),
    lists:zip( Seqs, Types ).


-spec is_tls( ip() ) -> boolean().
is_tls( IP ) ->
    TypedSeqs = get_typed_seqs( IP ),
    check_seqs_are_abba( TypedSeqs ).

is_ssl( IP ) ->
    TypedSeqs = get_typed_seqs( IP ),
    check_seqs_are_ssl( TypedSeqs ).

-spec solve( string(), fun( ( ip() ) -> boolean() ) ) -> non_neg_integer().
solve( Input, Filter ) ->
    IPs = string:tokens( Input, "\n" ),
    TlsIPs = lists:filter( Filter, IPs ),
    erlang:length( TlsIPs ).

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    solve( Input, fun is_tls/1 ).

-spec solve2( string() ) -> non_neg_integer().
solve2( Input ) ->
    solve( Input, fun is_ssl/1 ).



-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 1, solve1( "abba[mnop]qrst" ) ),
      ?_assertEqual( 0, solve1( "abcd[bddb]xyyx" ) ),
      ?_assertEqual( 0, solve1( "aaaa[qwer]tyui" ) ),
      ?_assertEqual( 1, solve1( "ioxxoj[asdfgh]zxcvbn" ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 1, solve2( "aba[bab]xyz" ) ),
      ?_assertEqual( 0, solve2( "xyx[xyx]xyx" ) ),
      ?_assertEqual( 1, solve2( "aaa[kek]eke" ) ),
      ?_assertEqual( 1, solve2( "zazbz[bzb]cdb" ) ) ].
