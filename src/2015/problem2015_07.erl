-module(problem2015_07).
-export([solve1/1, solve2/1]).

make_wires_map( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:foldl( fun( Line, Map ) ->
                         case string:tokens( Line, " ->" ) of
                             [ Source, Dest ] -> Map#{ Dest => { Source } };
                             [ UnaryOp, Source, Dest ] -> Map#{ Dest => { UnaryOp, Source } };
                             [ Source1, BinaryOp, Source2, Dest ] -> Map#{ Dest => { BinaryOp, Source1, Source2 } }
                         end
                 end,
                 #{},
                 Lines ).

parse_wire_val( Wire ) ->
    case string:to_integer( Wire ) of
        { Signal, [] } -> { signal, Signal };
        { error, _ } -> { wire, Wire }
    end.

run_instruction( { Source }, WiresMap ) ->
    get_wire_signal( Source, WiresMap );
run_instruction( { UnaryOp, Source }, WiresMap ) ->
    { S, WiresMap2 } = get_wire_signal( Source, WiresMap ),
    Op =
        case UnaryOp of
            "NOT" -> fun erlang:'bnot'/1
        end,
    { Op( S ), WiresMap2 };
run_instruction( { BinaryOp, Source1, Source2 }, WiresMap ) ->
    { S1, WiresMap2 } = get_wire_signal( Source1, WiresMap ),
    { S2, WiresMap3 } = get_wire_signal( Source2, WiresMap2 ),
    Op =
        case BinaryOp of
        "LSHIFT" -> fun erlang:'bsl'/2;
        "RSHIFT" -> fun erlang:'bsr'/2;
        "AND" -> fun erlang:'band'/2;
        "OR" -> fun erlang:'bor'/2
        end,
    { Op( S1, S2 ), WiresMap3 }.

get_wire_signal( Wire, WiresMap ) ->
    case parse_wire_val( Wire ) of
        { signal, Signal } -> { Signal, WiresMap };
        { wire, Wire } ->
            Instruction = maps:get( Wire, WiresMap ),
            { Signal, WiresMap2 } = run_instruction( Instruction, WiresMap ),
            { Signal, WiresMap2#{ Wire => { erlang:integer_to_list( Signal ) } } }
    end.

get_signal_a( WiresMap ) ->
    { SignalA, _ } = get_wire_signal( "a", WiresMap ),
    SignalA.

solve1( Input ) ->
    WiresMap = make_wires_map( Input ),
    get_signal_a( WiresMap ).

solve2( Input ) ->
    WiresMap = make_wires_map( Input ),
    SignalA = get_signal_a( WiresMap ),

    NewWiresMap = WiresMap#{ "b" => { erlang:integer_to_list( SignalA ) } },
    get_signal_a( NewWiresMap ).


-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 42, solve1( "42 -> a" ) ),
      ?_assertEqual( 42, solve1( "42 -> x
                                  x  -> a" ) ),
      ?_assertEqual( 2, solve1( "2 -> x
                                 6 -> y
                                 x AND y -> b
                                 b AND b -> a" ) ) ].
