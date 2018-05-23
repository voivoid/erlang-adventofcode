-module(problem2017_tests).
-include_lib("eunit/include/eunit.hrl").

problem_01_1() -> utils:make_test( "2017_01", fun problem2017_01:solve1/1, 1034 ).
problem_01_2() -> utils:make_test( "2017_01", fun problem2017_01:solve2/1, 1356 ).

problem_02_1() -> utils:make_test( "2017_02", fun problem2017_02:solve1/1, 58975 ).
problem_02_2() -> utils:make_test( "2017_02", fun problem2017_02:solve2/1, 308 ).

problem_03_1() -> utils:make_test( "2017_03", fun problem2017_03:solve1/1, 438 ).

problem_04_1() -> utils:make_test( "2017_04", fun problem2017_04:solve1/1, 455 ).
problem_04_2() -> utils:make_test( "2017_04", fun problem2017_04:solve2/1, 186 ).

problem_05_1() -> utils:make_test( "2017_05", fun problem2017_05:solve1/1, 355965 ).
problem_05_2() -> utils:make_test( "2017_05", fun problem2017_05:solve2/1, 26948068 ).

problem_06_1() -> utils:make_test( "2017_06", fun problem2017_06:solve1/1, 3156 ).
problem_06_2() -> utils:make_test( "2017_06", fun problem2017_06:solve2/1, 1610 ).

problem_07_1() -> utils:make_test( "2017_07", fun problem2017_07:solve1/1, "dgoocsw" ).
problem_07_2() -> utils:make_test( "2017_07", fun problem2017_07:solve2/1, 1275 ).

problem_08_1() -> utils:make_test( "2017_08", fun problem2017_08:solve1/1, 5752 ).
problem_08_2() -> utils:make_test( "2017_08", fun problem2017_08:solve2/1, 6366 ).

problem_09_1() -> utils:make_test( "2017_09", fun problem2017_09:solve1/1, 10800 ).
problem_09_2() -> utils:make_test( "2017_09", fun problem2017_09:solve2/1, 4522 ).

problem_10_1() -> utils:make_test( "2017_10", fun problem2017_10:solve1/1, 826 ).
problem_10_2() -> utils:make_test( "2017_10", fun problem2017_10:solve2/1, "d067d3f14d07e09c2e7308c3926605c4" ).

problem_11_1() -> utils:make_test( "2017_11", fun problem2017_11:solve1/1, 682 ).
problem_11_2() -> utils:make_test( "2017_11", fun problem2017_11:solve2/1, 1406 ).

problem_12_1() -> utils:make_test( "2017_12", fun problem2017_12:solve1/1, 283 ).
problem_12_2() -> utils:make_test( "2017_12", fun problem2017_12:solve2/1, 195 ).

problem_13_1() -> utils:make_test( "2017_13", fun problem2017_13:solve1/1, 1632 ).
problem_13_2() -> utils:make_test( "2017_13", fun problem2017_13:solve2/1, 3834136 ).

problem_14_1() -> utils:make_test( "2017_14", fun problem2017_14:solve1/1, 8140 ).
problem_14_2() -> utils:make_test( "2017_14", fun problem2017_14:solve2/1, 1182 ).

problem_15_1() -> utils:make_test( "2017_15", fun problem2017_15:solve1/1, 631 ).
problem_15_2() -> utils:make_test( "2017_15", fun problem2017_15:solve2/1, 279 ).

problem_16_1() -> utils:make_test( "2017_16", fun problem2017_16:solve1/1, "iabmedjhclofgknp" ).
problem_16_2() -> utils:make_test( "2017_16", fun problem2017_16:solve2/1, "oildcmfeajhbpngk" ).

problem_17_1() -> utils:make_test( "2017_17", fun problem2017_17:solve1/1, 640 ).
problem_17_2() -> utils:make_test( "2017_17", fun problem2017_17:solve2/1, 47949463 ).

problem_18_1() -> utils:make_test( "2017_18", fun problem2017_18:solve1/1, 3423 ).
problem_18_2() -> utils:make_test( "2017_18", fun problem2017_18:solve2/1, 7493 ).




problem2017_test_() ->
    { inparallel, [ problem_01_1(), problem_01_2(),
                    problem_02_1(), problem_02_2(),
                    problem_03_1(),
                    problem_04_1(), problem_04_2(),
                    problem_05_1(), problem_05_2(),
                    problem_06_1(), problem_06_2(),
                    problem_07_1(), problem_07_2(),
                    problem_08_1(), problem_08_2(),
                    problem_09_1(), problem_09_2(),
                    problem_10_1(), problem_10_2(),
                    problem_11_1(), problem_11_2(),
                    problem_12_1(), problem_12_2(),
                    problem_13_1(), problem_13_2(),
                    problem_14_1(), problem_14_2(),
                    problem_15_1(), problem_15_2(),
                    problem_16_1(), problem_16_2(),
                    problem_17_1(), problem_17_2(),
                    problem_18_1(), problem_18_2() ] }.
