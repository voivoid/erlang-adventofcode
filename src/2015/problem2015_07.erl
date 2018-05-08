-module(problem2015_07).
-export([solve1/1]).

make_wires_map( Lines ) ->
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
    case parse_wire_val( Source ) of
        { signal, Signal } -> Signal;
        { wire, Source } -> get_wire_signal( Source, WiresMap )
    end;
run_instruction( { UnaryOp, Source }, WiresMap ) -> 
    S = get_wire_signal( Source, WiresMap ),
    Op = 
        case UnaryOp of
            "NOT" -> fun erlang:'bnot'/1
        end,
    Op( S );
run_instruction( { BinaryOp, Source1, Source2 }, WiresMap ) -> 
    S1 = get_wire_signal( Source1, WiresMap ),
    S2 = get_wire_signal( Source2, WiresMap ),
    Op = 
        case BinaryOp of
        "LSHIFT" -> fun erlang:'bsl'/2;
        "RSHIFT" -> fun erlang:'bsr'/2;
        "AND" -> fun erlang:'band'/2;
        "OR" -> fun erlang:'bor'/2
        end,
    Op( S1, S2 ).

get_wire_signal( Wire, WiresMap ) ->
    Instruction = maps:get( Wire, WiresMap ),
    run_instruction( Instruction, WiresMap ).


solve1( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    WiresMap = make_wires_map( Lines ),
    get_wire_signal( "a", WiresMap ).


-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    [ ?_assertEqual( 42, solve1( "42 -> a" ) ),
      ?_assertEqual( 42, solve1( "42 -> x
                                  x  -> a" ) ) ].
