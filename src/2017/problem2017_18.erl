-module(problem2017_18).
-export([solve1/1, solve2/1]).


-type reg() :: string().
-type num() :: integer().
-type val() :: reg() | num().
-type reg_map() :: #{ reg() := num() }.
-type offset() :: integer().
-type cmd_runner() :: fun( ( cmd(), reg_map() ) -> unhandled | { reg_map(), offset() } ).

-type cmd() :: { set, reg(), val() } |
               { add, reg(), val() } |
               { mul, reg(), val() } |
               { mod, reg(), val() } |
               { snd, val() } |
               { rcv, val() } |
               { jgz, val(), val() }.
-type cmds() :: zipper:zipper( cmd() ).

-spec parse_val( string() ) -> val().
parse_val( Value ) ->
    case string:to_integer( Value ) of
        { Int, [] } -> Int;
        _ -> Value
    end.

-spec parse_command( [ string() ] ) -> cmd().
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

-spec parse_commands( string() ) -> cmds().
parse_commands( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    ParsedCmds = lists:map( fun( Line ) -> parse_command( string:tokens( Line, " " ) ) end, Lines ),
    zipper:from_list( ParsedCmds ).

-spec get_num( val(), reg_map() ) -> num().
get_num( Val, _ ) when erlang:is_integer( Val ) ->
    Val;
get_num( Val, RegMap ) ->
    case maps:find( Val, RegMap ) of
        {ok, Num} -> Num;
        _ -> 0
    end.

-spec stop_jmp_offset() -> offset().
stop_jmp_offset() ->
    99999.

-spec update_regmap( reg(), num(), reg_map() ) -> reg_map().
update_regmap( Reg, Num, RegMap ) ->
    RegMap#{ Reg => Num }.

-define(DEFAULT_JMP_OFFSET, 1).
-define(RESULT_REG, "result").

-spec run_common_command( cmd(), reg_map() ) -> { reg_map(), offset() }.
run_common_command( { set, Reg, Val }, RegMap ) ->
    { update_regmap( Reg, get_num( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_common_command( { add, Reg, Val }, RegMap ) ->
    { update_regmap( Reg, get_num( Reg, RegMap ) + get_num( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_common_command( { mul, Reg, Val}, RegMap ) ->
    { update_regmap( Reg, get_num( Reg, RegMap ) * get_num( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_common_command( { mod, Reg, Val}, RegMap ) ->
    { update_regmap( Reg, get_num( Reg, RegMap ) rem get_num( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_common_command( { jgz, X, Y }, RegMap ) ->
    Offset = case get_num( X, RegMap ) of
                 XVal when XVal > 0 -> get_num( Y, RegMap );
                 _ -> ?DEFAULT_JMP_OFFSET
             end,
    { RegMap, Offset }.

-spec run_command( cmd(), reg_map(), cmd_runner() ) -> { reg_map(), offset() }.
run_command( Cmd, RegMap, CmdRunner ) ->
    case CmdRunner( Cmd, RegMap ) of
        unhandled -> run_common_command( Cmd, RegMap );
        Res -> Res
    end.

-spec run_commands( cmds(), reg_map(), cmd_runner() ) -> num().
run_commands( ZipperCmds, RegMap, CmdRunner ) ->
    Cmd = zipper:get( ZipperCmds ),
    { UpdatedRegMap, JumpOffset } = run_command( Cmd, RegMap, CmdRunner ),

    NextCmdNum = zipper:pos( ZipperCmds ) + JumpOffset,
    IsLastCmd = ( NextCmdNum > zipper:len( ZipperCmds ) ) or ( NextCmdNum < 1 ),

    case IsLastCmd of
        true -> get_num( ?RESULT_REG, UpdatedRegMap );
        false -> run_commands( zipper:next_n( JumpOffset, ZipperCmds ), UpdatedRegMap, CmdRunner )
    end.

-spec solve1( string() ) -> num().
solve1( Input ) ->
    ZipperCmds = parse_commands( Input ),

    CmdRunner = fun( { snd, Val }, RegMap ) ->
                        { update_regmap( ?RESULT_REG, get_num( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
                   ( { rcv, X }, RegMap ) ->
                        case get_num( X, RegMap ) of
                            XVal when XVal /= 0 -> { RegMap, stop_jmp_offset() };
                            _ -> { RegMap, ?DEFAULT_JMP_OFFSET }
                        end;
                   ( _, _ ) -> unhandled
                end,

    run_commands( ZipperCmds, #{}, CmdRunner ).

-spec make_part2_cmd_runner( pid() ) -> cmd_runner().
make_part2_cmd_runner( Pid ) ->
    fun( { snd, Val }, RegMap ) ->
            SndCounter = get_num( ?RESULT_REG, RegMap ),
            Pid ! get_num( Val, RegMap ),
            UpdatedRegMap = update_regmap( ?RESULT_REG, SndCounter + 1, RegMap ),
            { UpdatedRegMap, ?DEFAULT_JMP_OFFSET };
       ( { rcv, X }, RegMap ) ->
            Val = receive V -> V
                  after 1000 -> deadlock
                  end,

            case Val of
                deadlock -> { RegMap, stop_jmp_offset() };
                _ -> { update_regmap( X, Val, RegMap ), ?DEFAULT_JMP_OFFSET }
            end;
       ( _, _ ) -> unhandled
    end.

-spec solve2( string() ) -> num().
solve2( Input ) ->
    ZipperCmds = parse_commands( Input ),

    Self = self(),
    P0 = spawn( fun() ->
                        P1 = receive P -> P end,
                        run_commands( ZipperCmds, #{ "p" => 0 }, make_part2_cmd_runner( P1 ) )
                end ),
    P1 = spawn( fun() ->
                        P0 = receive P -> P end,
                        Res = run_commands( ZipperCmds, #{ "p" => 1 }, make_part2_cmd_runner( P0 ) ),
                        Self ! Res
                end ),

    P0 ! P1,
    P1 ! P0,

    Result = receive R -> R end,
    Result.

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
    [ ?_assertEqual( 4, solve1( Input ) ) ].

solve2_test_() ->
    Input = "snd 1
             snd 2
             snd p
             rcv a
             rcv b
             rcv c
             rcv d",
    [ ?_assertEqual(3 , solve2( Input ) ) ].
