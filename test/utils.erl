-module(utils).
-export([make_test/3]).


-include_lib("eunit/include/eunit.hrl").

load_input( FilePath ) ->
    {ok, Data} = file:read_file( FilePath ),
    binary:bin_to_list( Data ).

make_test(InputFileName, Func, Expected) ->
    TestInputFile = "test/inputs/" ++ InputFileName,
    [ TestName ] = io_lib:format( "~p", [ Func ] ),
    { setup, fun() -> load_input( TestInputFile ) end, fun ( Input ) -> { TestName, timeout, 120, ?_assertEqual( Expected, Func( Input ) ) } end }.

