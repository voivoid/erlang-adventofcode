-module(problem2016_tests).
-include_lib("eunit/include/eunit.hrl").



problem_01_1_test_() -> utils:make_test( "2016_01", fun problem2016_01:solve1/1, 273 ).
problem_01_2_test_() -> utils:make_test( "2016_01", fun problem2016_01:solve2/1, 115 ).

problem_02_1_test_() -> utils:make_test( "2016_02", fun problem2016_02:solve1/1, "78293" ).
problem_02_2_test_() -> utils:make_test( "2016_02", fun problem2016_02:solve2/1, "AC8C8" ).

problem_03_1_test_() -> utils:make_test( "2016_03", fun problem2016_03:solve1/1, 983 ).
problem_03_2_test_() -> utils:make_test( "2016_03", fun problem2016_03:solve2/1, 1836 ).

problem_04_1_test_() -> utils:make_test( "2016_04", fun problem2016_04:solve1/1, 278221 ).
problem_04_2_test_() -> utils:make_test( "2016_04", fun problem2016_04:solve2/1, 267 ).

problem_06_1_test_() -> utils:make_test( "2016_06", fun problem2016_06:solve1/1, "wkbvmikb" ).
problem_06_2_test_() -> utils:make_test( "2016_06", fun problem2016_06:solve2/1, "evakwaga" ).
