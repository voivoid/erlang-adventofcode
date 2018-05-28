-module(problem2016_12).
-export([solve1/1, solve2/1]).

-type reg() :: string().
-type num() :: integer().
-type val() :: reg() | num().
-type reg_map() :: #{ reg() := num() }.
-type offset() :: integer().

-type cmd() :: { cpy, val(), reg() } |
               { inc, reg() } |
               { dec, reg() } |
               { jnz, val(), val() }.
-type cmds() :: zipper:zipper( cmd() ).

%%% COMMON

-spec parse_val( string() ) -> val().
parse_val( Value ) ->
    case string:to_integer( Value ) of
        { Int, [] } -> Int;
        _ -> Value
    end.

-spec parse_command( [ string() ] ) -> cmd().
parse_command( [ "cpy", Val, Reg ] ) ->
    { cpy, parse_val( Val ), Reg };
parse_command( [ "inc", Reg ] ) ->
    { inc, Reg };
parse_command( [ "dec", Reg ] ) ->
    { dec, Reg };
parse_command( [ "jnz", Val1, Val2 ] ) ->
    { jnz, parse_val( Val1 ), parse_val( Val2 ) }.

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

-spec update_regmap( reg(), num(), reg_map() ) -> reg_map().
update_regmap( Reg, Num, RegMap ) ->
    RegMap#{ Reg => Num }.

-define(DEFAULT_JMP_OFFSET, 1).

-spec run_command( cmd(), reg_map() ) -> { reg_map(), offset() }.
run_command( { cpy, Val, Reg }, RegMap ) ->
    { update_regmap( Reg, get_num( Val, RegMap ), RegMap ), ?DEFAULT_JMP_OFFSET };
run_command( { inc, Reg }, RegMap ) ->
    { update_regmap( Reg, get_num( Reg, RegMap ) + 1, RegMap ), ?DEFAULT_JMP_OFFSET };
run_command( { dec, Reg}, RegMap ) ->
    { update_regmap( Reg, get_num( Reg, RegMap ) - 1, RegMap ), ?DEFAULT_JMP_OFFSET };
run_command( { jnz, X, Y }, RegMap ) ->
    Offset = case get_num( X, RegMap ) of
                 XVal when XVal /= 0 -> get_num( Y, RegMap );
                 _ -> ?DEFAULT_JMP_OFFSET
             end,
    { RegMap, Offset }.

-spec run_commands( cmds(), reg_map() ) -> num().
run_commands( ZipperCmds, RegMap ) ->
    Cmd = zipper:get( ZipperCmds ),
    { UpdatedRegMap, JumpOffset } = run_command( Cmd, RegMap ),

    NextCmdNum = zipper:pos( ZipperCmds ) + JumpOffset,
    IsLastCmd = ( NextCmdNum > zipper:len( ZipperCmds ) ) or ( NextCmdNum < 1 ),

    case IsLastCmd of
        true -> get_num( "a", UpdatedRegMap );
        false -> run_commands( zipper:next_n( JumpOffset, ZipperCmds ), UpdatedRegMap )
    end.

%%% PART 1

-spec solve1( string() ) -> num().
solve1( Input ) ->
    ZipperCmds = parse_commands( Input ),
    run_commands( ZipperCmds, #{} ).

%%% PART 2

solve2( Input ) ->
    ZipperCmds = parse_commands( Input ),
    run_commands( ZipperCmds, #{ "c" => 1 } ).

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

solve1_test_() ->
    Input = "cpy 41 a
             inc a
             inc a
             dec a
             jnz a 2
             dec a",
    [ ?_assertEqual( 42, solve1( Input ) ) ].
