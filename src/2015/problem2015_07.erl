-module(problem2015_07).
-export([solve1/1, solve2/1]).

-type signal() :: integer().
-type id() :: string().
-type source() :: { signal, signal() } | { wire, id() }.
-type unary_ops() :: not_op.
-type binary_ops() :: lshift_op | rshift_op | and_op | or_op.
-type instruction() :: { source() } | { unary_ops(), source() } | { binary_ops(), source(), source() }.
-type wires_map() :: #{ id() := instruction() }.

%%% COMMON

-spec parse_source( string() ) -> source().
parse_source( Wire ) ->
    case string:to_integer( Wire ) of
        { Signal, [] } -> { signal, Signal };
        { error, _ } -> { wire, Wire }
    end.

-spec parse_op( string() ) -> unary_ops() | binary_ops().
parse_op( "NOT"    ) -> not_op;
parse_op( "LSHIFT" ) -> lshift_op;
parse_op( "RSHIFT" ) -> rshift_op;
parse_op( "AND"    ) -> and_op;
parse_op( "OR"     ) -> or_op.

-spec make_wires_map( string() ) -> wires_map().
make_wires_map( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:foldl( fun( Line, Map ) ->
                         case string:tokens( Line, " ->" ) of
                             [ Source, Dest ] -> Map#{ Dest => { parse_source( Source ) } };
                             [ UnaryOp, Source, Dest ] -> Map#{ Dest => { parse_op( UnaryOp ), parse_source( Source ) } };
                             [ Source1, BinaryOp, Source2, Dest ] -> Map#{ Dest => { parse_op( BinaryOp ), parse_source( Source1 ), parse_source( Source2 ) } }
                         end
                 end,
                 #{},
                 Lines ).

-spec get_source_val( source(), wires_map() ) -> { signal(), wires_map() }.
get_source_val( { signal, Source }, WiresMap ) -> { Source, WiresMap };
get_source_val( { wire, Wire }, WiresMap ) -> get_wire_signal( Wire, WiresMap ).

-spec run_instruction( instruction(), wires_map() ) -> { signal(), wires_map() }.
run_instruction( { Source }, WiresMap ) ->
    get_source_val( Source, WiresMap );
run_instruction( { UnaryOp, Source }, WiresMap ) ->
    { S, WiresMap2 } = get_source_val( Source, WiresMap ),
    Op =
        case UnaryOp of
            not_op -> fun( N ) -> 16#FFFF band ( bnot N ) end
        end,
    { Op( S ), WiresMap2 };
run_instruction( { BinaryOp, Source1, Source2 }, WiresMap ) ->
    { S1, WiresMap2 } = get_source_val( Source1, WiresMap ),
    { S2, WiresMap3 } = get_source_val( Source2, WiresMap2 ),
    Op =
        case BinaryOp of
            lshift_op -> fun erlang:'bsl'/2;
            rshift_op -> fun erlang:'bsr'/2;
            and_op -> fun erlang:'band'/2;
            or_op -> fun erlang:'bor'/2
        end,
    { Op( S1, S2 ), WiresMap3 }.

-spec get_wire_signal( id(), wires_map() ) -> { signal(), wires_map() }.
get_wire_signal( WireId, WiresMap ) ->
    Instruction = maps:get( WireId, WiresMap ),
    { Signal, WiresMap2 } = run_instruction( Instruction, WiresMap ),
    { Signal, WiresMap2#{ WireId => { { signal, Signal } } } }.

-spec get_signal_a( wires_map() ) -> signal().
get_signal_a( WiresMap ) ->
    { SignalA, _ } = get_wire_signal( "a", WiresMap ),
    SignalA.

%%% PART 1

-spec solve1( string() ) -> integer().
solve1( Input ) ->
    WiresMap = make_wires_map( Input ),
    get_signal_a( WiresMap ).

%%% PART 2

-spec solve2( string() ) -> integer().
solve2( Input ) ->
    WiresMap = make_wires_map( Input ),
    SignalA = get_signal_a( WiresMap ),

    NewWiresMap = WiresMap#{ "b" => { { signal, SignalA } } },
    get_signal_a( NewWiresMap ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

test_input() ->
    "123 -> x
     456 -> y
     x AND y -> d
     x OR y -> e
     x LSHIFT 2 -> f
     y RSHIFT 2 -> g
     NOT x -> h
     NOT y -> i".

test_wire( Wire ) ->
    WiresMap = make_wires_map( test_input() ),
    { Signal, _ } = get_wire_signal( Wire, WiresMap ),
    Signal.

solve1_test_() ->
    [
     ?_assertEqual( 72,    test_wire( "d" ) ),
     ?_assertEqual( 507,   test_wire( "e" ) ),
     ?_assertEqual( 492,   test_wire( "f" ) ),
     ?_assertEqual( 114,   test_wire( "g" ) ),
     ?_assertEqual( 65412, test_wire( "h" ) ),
     ?_assertEqual( 65079, test_wire( "i" ) ),
     ?_assertEqual( 123,   test_wire( "x" ) ),
     ?_assertEqual( 456,   test_wire( "y" ) )
    ].
