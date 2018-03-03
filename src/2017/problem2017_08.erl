-module(problem2017_08).
-export([solve1/1, solve2/1]).

-type reg_val() :: integer().
-type reg() :: string().
-type reg_map() :: #{ reg() := reg_val() }.
-type cmdop() :: fun( ( reg_val(), reg_val() ) -> reg_val() ).
-type condop() :: fun( ( reg_val(), reg_val() ) -> boolean() ).
-type reg_state() :: { reg_map(), reg_val() }.
-type instruction() :: { reg(), cmdop(), reg_val(), reg(), condop(), reg_val() }.

-spec parse_instruction( string() ) -> instruction().
parse_instruction( Line ) ->
    [ Op1Reg, Cmd, Op2Val, _, Reg, Cond, CondVal ] = string:tokens( Line, " " ),
    CondOp = case Cond of
                 "<" -> fun erlang:'<'/2;
                 ">" -> fun erlang:'>'/2;
                 ">=" -> fun erlang:'>='/2;
                 "<=" -> fun erlang:'=<'/2;
                 "==" -> fun erlang:'=='/2;
                 "!=" -> fun erlang:'/='/2
             end,
    CmdOp = case Cmd of
                "inc" -> fun erlang:'+'/2;
                "dec" -> fun erlang:'-'/2
            end,

    { Op1Reg, CmdOp, erlang:list_to_integer( Op2Val ), Reg, CondOp, erlang:list_to_integer( CondVal ) }.

-spec get_val( reg(), reg_map() ) -> reg_val().
get_val( Reg, Registers ) ->
    maps:get( Reg, Registers, 0 ).

-spec check_cond( reg(), condop(), reg_val(), reg_map() ) -> boolean().
check_cond( Reg, CondOp, CondVal, Registers ) ->
    RegVal = get_val( Reg, Registers),
    CondOp( RegVal, CondVal ).

-spec run_command( reg(), cmdop(), reg_val(), reg_state() ) -> reg_state().
run_command( Op1Reg, CmdOp, Op2Val, { Registers, MaxRegEver } ) ->
    Op1Val = get_val( Op1Reg, Registers ),
    Res = CmdOp( Op1Val, Op2Val ),
    { Registers#{ Op1Reg => Res }, erlang:max( Res, MaxRegEver ) }.


-spec run_instruction( instruction(), reg_state() ) -> reg_state().
run_instruction( { Op1Reg, CmdOp, Op2Val, Reg, CondOp, CondVal }, { Registers, _ } = RegistersState ) ->
    case check_cond( Reg, CondOp, CondVal, Registers ) of
        true -> run_command( Op1Reg, CmdOp, Op2Val, RegistersState );
        false -> RegistersState
    end.    


-spec solve( string() ) -> { reg_map(), reg_val() }.
solve( Input ) ->
    Instructions = lists:map( fun parse_instruction/1, ( string:tokens( Input, "\n" ) ) ),
    lists:foldl( fun run_instruction/2, { #{}, 0 }, Instructions ).

-spec solve1( string() ) -> reg_val().
solve1( Input ) ->
    { Registers, _ } = solve( Input ),
    lists:max( maps:values( Registers ) ).


-spec solve2( string() ) -> reg_val().
solve2( Input ) ->
    { _, MaxRegEver } = solve( Input ),
    MaxRegEver.
