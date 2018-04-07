-module( problem2015_tests ).
-include_lib("eunit/include/eunit.hrl").


problem_01_1_test_() -> utils:make_test( "2015_01", fun problem2015_01:solve1/1, 280 ).
problem_01_2_test_() -> utils:make_test( "2015_01", fun problem2015_01:solve2/1, 1797 ).

problem_02_1_test_() -> utils:make_test( "2015_02", fun problem2015_02:solve1/1, 1586300 ).
problem_02_2_test_() -> utils:make_test( "2015_02", fun problem2015_02:solve2/1, 3737498 ).

problem_03_1_test_() -> utils:make_test( "2015_03", fun problem2015_03:solve1/1, 2592 ).
problem_03_2_test_() -> utils:make_test( "2015_03", fun problem2015_03:solve2/1, 2360 ).

problem_05_1_test_() -> utils:make_test( "2015_05", fun problem2015_05:solve1/1, 255 ).
problem_05_2_test_() -> utils:make_test( "2015_05", fun problem2015_05:solve2/1, 55 ).
