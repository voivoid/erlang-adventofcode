-module( problem2015_tests ).
-include_lib("eunit/include/eunit.hrl").

problem_01_1() -> utils:make_test( "2015_01", fun problem2015_01:solve1/1, 280 ).
problem_01_2() -> utils:make_test( "2015_01", fun problem2015_01:solve2/1, 1797 ).

problem_02_1() -> utils:make_test( "2015_02", fun problem2015_02:solve1/1, 1586300 ).
problem_02_2() -> utils:make_test( "2015_02", fun problem2015_02:solve2/1, 3737498 ).

problem_03_1() -> utils:make_test( "2015_03", fun problem2015_03:solve1/1, 2592 ).
problem_03_2() -> utils:make_test( "2015_03", fun problem2015_03:solve2/1, 2360 ).

problem_04_1() -> utils:make_test( "2015_04", fun problem2015_04:solve1/1, 254575 ).
problem_04_2() -> utils:make_test( "2015_04", fun problem2015_04:solve2/1, 1038736 ).

problem_05_1() -> utils:make_test( "2015_05", fun problem2015_05:solve1/1, 255 ).
problem_05_2() -> utils:make_test( "2015_05", fun problem2015_05:solve2/1, 55 ).

problem_07_1() -> utils:make_test( "2015_07", fun problem2015_07:solve1/1, 46065 ).
problem_07_2() -> utils:make_test( "2015_07", fun problem2015_07:solve2/1, 14134 ).

problem_08_1() -> utils:make_test( "2015_08", fun problem2015_08:solve1/1, 1333 ).
problem_08_2() -> utils:make_test( "2015_08", fun problem2015_08:solve2/1, 2046 ).

problem_09_1() -> utils:make_test( "2015_09", fun problem2015_09:solve1/1, 207 ).
problem_09_2() -> utils:make_test( "2015_09", fun problem2015_09:solve2/1, 804 ).

problem_10_1() -> utils:make_test( "2015_10", fun problem2015_10:solve1/1, 252594 ).
problem_10_2() -> utils:make_test( "2015_10", fun problem2015_10:solve2/1, 3579328 ).

problem_11_1() -> utils:make_test( "2015_11", fun problem2015_11:solve1/1, "hxbxxyzz" ).
problem_11_2() -> utils:make_test( "2015_11", fun problem2015_11:solve2/1, "hxcaabcc" ).

problem_12_1() -> utils:make_test( "2015_12", fun problem2015_12:solve1/1, 119433 ).
problem_12_2() -> utils:make_test( "2015_12", fun problem2015_12:solve2/1, 68466 ).

problem_13_1() -> utils:make_test( "2015_13", fun problem2015_13:solve1/1, 618 ).
problem_13_2() -> utils:make_test( "2015_13", fun problem2015_13:solve2/1, 601 ).

problem_14_1() -> utils:make_test( "2015_14", fun problem2015_14:solve1/1, 2660 ).
problem_14_2() -> utils:make_test( "2015_14", fun problem2015_14:solve2/1, 1256 ).

problem_15_1() -> utils:make_test( "2015_15", fun problem2015_15:solve1/1, 13882464 ).
problem_15_2() -> utils:make_test( "2015_15", fun problem2015_15:solve2/1, 11171160 ).

problem_16_1() -> utils:make_test( "2015_16", fun problem2015_16:solve1/1, 103 ).
problem_16_2() -> utils:make_test( "2015_16", fun problem2015_16:solve2/1, 405 ).

problem_17_1() -> utils:make_test( "2015_17", fun problem2015_17:solve1/1, 4372 ).
problem_17_2() -> utils:make_test( "2015_17", fun problem2015_17:solve2/1, 4 ).

problem_18_1() -> utils:make_test( "2015_18", fun problem2015_18:solve1/1, 0 ).


problem2015_test_() ->
    { inparallel, [
                   problem_01_1(), problem_01_2(),
                   problem_02_1(), problem_02_2(),
                   problem_03_1(), problem_03_2(),
                   problem_04_1(), problem_04_2(),
                   problem_05_1(), problem_05_2(),
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
                   problem_18_1()
                  ] }.
