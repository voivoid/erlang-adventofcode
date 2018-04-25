-module(problem2016_tests).
-include_lib("eunit/include/eunit.hrl").



problem_01_1_test_() -> utils:make_test( "2016_01", fun problem2016_01:solve1/1, 273 ).
problem_01_2_test_() -> utils:make_test( "2016_01", fun problem2016_01:solve2/1, 115 ).

problem_02_1_test_() -> utils:make_test( "2016_02", fun problem2016_02:solve1/1, "78293" ).
problem_02_2_test_() -> utils:make_test( "2016_02", fun problem2016_02:solve2/1, "AC8C8" ).
