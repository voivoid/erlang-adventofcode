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

problem_07_1_test_() -> utils:make_test( "2015_07", fun problem2015_07:solve1/1, 46065 ).
problem_07_2_test_() -> utils:make_test( "2015_07", fun problem2015_07:solve2/1, 14134 ).

problem_08_1_test_() -> utils:make_test( "2015_08", fun problem2015_08:solve1/1, 1333 ).
problem_08_2_test_() -> utils:make_test( "2015_08", fun problem2015_08:solve2/1, 2046 ).

problem_10_1_test_() -> utils:make_test( "2015_10", fun problem2015_10:solve1/1, 252594 ).
problem_10_2_test_() -> utils:make_test( "2015_10", fun problem2015_10:solve2/1, 3579328 ).

problem_11_1_test_() -> utils:make_test( "2015_11", fun problem2015_11:solve1/1, "hxbxxyzz" ).
problem_11_2_test_() -> utils:make_test( "2015_11", fun problem2015_11:solve2/1, "hxcaabcc" ).
