-module( problem2017_tests ).
-include_lib("eunit/include/eunit.hrl").

problem_01_1_test_() -> utils:make_test( "2017_01", fun problem2017_01:solve1/1, 1034 ).
problem_01_2_test_() -> utils:make_test( "2017_01", fun problem2017_01:solve2/1, 1356 ).

problem_02_1_test_() -> utils:make_test( "2017_02", fun problem2017_02:solve1/1, 58975 ).
problem_02_2_test_() -> utils:make_test( "2017_02", fun problem2017_02:solve2/1, 308 ).

problem_04_1_test_() -> utils:make_test( "2017_04", fun problem2017_04:solve1/1, 455 ).
problem_04_2_test_() -> utils:make_test( "2017_04", fun problem2017_04:solve2/1, 186 ).

problem_05_1_test_() -> utils:make_test( "2017_05", fun problem2017_05:solve1/1, 355965 ).
problem_05_2_test_() -> utils:make_test( "2017_05", fun problem2017_05:solve2/1, 26948068 ).

problem_06_1_test_() -> utils:make_test( "2017_06", fun problem2017_06:solve1/1, 3156 ).
problem_06_2_test_() -> utils:make_test( "2017_06", fun problem2017_06:solve2/1, 1610 ).

problem_07_1_test_() -> utils:make_test( "2017_07", fun problem2017_07:solve1/1, "dgoocsw" ).
problem_07_2_test_() -> utils:make_test( "2017_07", fun problem2017_07:solve2/1, 1275 ).

problem_08_1_test_() -> utils:make_test( "2017_08", fun problem2017_08:solve1/1, 5752 ).
problem_08_2_test_() -> utils:make_test( "2017_08", fun problem2017_08:solve2/1, 6366 ).

problem_09_1_test_() -> utils:make_test( "2017_09", fun problem2017_09:solve1/1, 10800 ).
problem_09_2_test_() -> utils:make_test( "2017_09", fun problem2017_09:solve2/1, 4522 ).

problem_10_1_test_() -> utils:make_test( "2017_10", fun problem2017_10:solve1/1, 826 ).
problem_10_2_test_() -> utils:make_test( "2017_10", fun problem2017_10:solve2/1, "d067d3f14d07e09c2e7308c3926605c4" ).

problem_11_1_test_() -> utils:make_test( "2017_11", fun problem2017_11:solve1/1, 682 ).
problem_11_2_test_() -> utils:make_test( "2017_11", fun problem2017_11:solve2/1, 1406 ).

problem_12_1_test_() -> utils:make_test( "2017_12", fun problem2017_12:solve1/1, 283 ).
problem_12_2_test_() -> utils:make_test( "2017_12", fun problem2017_12:solve2/1, 195 ).

problem_13_1_test_() -> utils:make_test( "2017_13", fun problem2017_13:solve1/1, 1632 ).
problem_13_2_test_() -> utils:make_test( "2017_13", fun problem2017_13:solve2/1, 3834136 ).

problem_14_1_test_() -> utils:make_test( "2017_14", fun problem2017_14:solve1/1, 8140 ).
