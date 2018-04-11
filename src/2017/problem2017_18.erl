-module(problem2017_18).
-export([solve1/1, solve2/1]).

parse_val( Value ) ->
    case string:to_integer( Value ) of
        { Int, [] } -> Int;
        _ -> Value
    end.

parse_command( [ "set", Reg, Val ] ) ->
    { set, Reg, parse_val( Val ) };
parse_command( [ "add", Reg, Val ] ) ->
    { add, Reg, parse_val( Val ) };
parse_command( [ "mul", Reg, Val ] ) ->
    { mul, Reg, parse_val( Val ) };
parse_command( [ "mod", Reg, Val ] ) ->
    { mod, Reg, parse_val( Val ) };
parse_command( [ "snd", Val ] ) ->
    { snd, parse_val( Val ) };
parse_command( [ "rcv", Val ] ) ->
    { rcv, parse_val( Val ) };
parse_command( [ "jgz", Val1, Val2 ] ) ->
    { jgz, parse_val( Val1 ), parse_val( Val2 ) }.

parse_commands( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    ParsedCmds = lists:map( fun( Line ) -> parse_command( string:tokens( Line, " " ) ) end, Lines ),
    zipper:from_list( ParsedCmds ).

get_val( Val, _ ) when erlang:is_integer( Val ) ->
    Val;
get_val( Reg, RegMap ) ->
    case maps:find( Reg, RegMap ) of
        {ok, Val} -> Val;
        _ -> 0
    end.

update_regmap( Reg, Val, RegMap ) ->
    RegMap#{ Reg => Val }.

-define(DEFAULT_JMP_OFFSET, 1).

run_common_command( { set, Reg, Val }, RegMap ) ->
    { update_regmap( Reg, get_val( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_common_command( { add, Reg, Val }, RegMap ) ->
    { update_regmap( Reg, get_val( Reg, RegMap ) + get_val( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_common_command( { mul, Reg, Val}, RegMap ) ->
    { update_regmap( Reg, get_val( Reg, RegMap ) * get_val( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_common_command( { mod, Reg, Val}, RegMap ) ->
    { update_regmap( Reg, get_val( Reg, RegMap ) rem get_val( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_common_command( { jgz, X, Y }, RegMap ) ->
    Offset = case get_val( X, RegMap ) of
                 XVal when XVal > 0 -> get_val( Y, RegMap );
                 _ -> ?DEFAULT_JMP_OFFSET
             end,
    { RegMap, Offset }.

run_command( Cmd, RegMap, CmdRunner ) ->
    case CmdRunner( Cmd, RegMap ) of
        unhandled -> run_common_command( Cmd, RegMap );
        Res -> Res
    end.

run_commands( ZipperCmds, RegMap, CmdRunner ) ->
    Cmd = zipper:get( ZipperCmds ),
    { UpdatedRegMap, JumpOffset } = run_command( Cmd, RegMap, CmdRunner ),

    NextCmdNum = zipper:pos( ZipperCmds ) + JumpOffset,
    IsLastCmd = ( NextCmdNum > zipper:len( ZipperCmds ) ) or ( NextCmdNum < 1 ),
    
    case IsLastCmd of
        true -> get_val( "snd", RegMap );
        false -> run_commands( zipper:next_n( JumpOffset, ZipperCmds ), UpdatedRegMap, CmdRunner )
    end.



solve1( Input ) ->
    ZipperCmds = parse_commands( Input ),

    CmdRunner = fun( { snd, Val }, RegMap ) ->
                       { update_regmap( "snd", get_val( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
                  ( { rcv, X }, RegMap ) ->
                       case get_val( X, RegMap ) of
                           XVal when XVal /= 0 -> { RegMap, zipper:len( ZipperCmds ) };
                           _ -> { RegMap, ?DEFAULT_JMP_OFFSET }
                       end;
                  ( _, _ ) -> unhandled
                end,
    
    run_commands( ZipperCmds, #{}, CmdRunner ).

solve2( Input ) ->
    ZipperCmds = parse_commands( Input ),
    
    CmdRunner = fun( { snd, _Val }, RegMap ) ->
                        RegMap;
                   ( { rcv, _X }, RegMap ) ->
                        RegMap;
                   ( _, _ ) -> unhandled
                end,
    
    _P0 = spawn( fun() -> run_commands( ZipperCmds, #{ "p" => 0 }, CmdRunner ) end ),
    _P1 = spawn( fun() -> run_commands( ZipperCmds, #{ "p" => 1 }, CmdRunner ) end ),
    0.

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    Input = "set a 1
             add a 2
             mul a a
             mod a 5
             snd a
             set a 0
             rcv a
             jgz a -1
             set a 1
             jgz a -2",
    [ ?_assertEqual( 4, solve1(Input) ) ].
