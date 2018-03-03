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
