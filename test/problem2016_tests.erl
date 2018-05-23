-module(problem2016_tests).
-include_lib("eunit/include/eunit.hrl").



problem_01_1() -> utils:make_test( "2016_01", fun problem2016_01:solve1/1, 273 ).
problem_01_2() -> utils:make_test( "2016_01", fun problem2016_01:solve2/1, 115 ).

problem_02_1() -> utils:make_test( "2016_02", fun problem2016_02:solve1/1, "78293" ).
problem_02_2() -> utils:make_test( "2016_02", fun problem2016_02:solve2/1, "AC8C8" ).

problem_03_1() -> utils:make_test( "2016_03", fun problem2016_03:solve1/1, 983 ).
problem_03_2() -> utils:make_test( "2016_03", fun problem2016_03:solve2/1, 1836 ).

problem_04_1() -> utils:make_test( "2016_04", fun problem2016_04:solve1/1, 278221 ).
problem_04_2() -> utils:make_test( "2016_04", fun problem2016_04:solve2/1, 267 ).

problem_05_1() -> utils:make_test( "2016_05", fun problem2016_05:solve1/1, "4543C154" ).
problem_05_2() -> utils:make_test( "2016_05", fun problem2016_05:solve2/1, "1050CBBD" ).

problem_06_1() -> utils:make_test( "2016_06", fun problem2016_06:solve1/1, "wkbvmikb" ).
problem_06_2() -> utils:make_test( "2016_06", fun problem2016_06:solve2/1, "evakwaga" ).

problem_07_1() -> utils:make_test( "2016_07", fun problem2016_07:solve1/1, 118 ).
problem_07_2() -> utils:make_test( "2016_07", fun problem2016_07:solve2/1, 260 ).

problem_09_1() -> utils:make_test( "2016_09", fun problem2016_09:solve1/1, 107035 ).
problem_09_2() -> utils:make_test( "2016_09", fun problem2016_09:solve2/1, 11451628995 ).


problem2016_test_() ->
    { inparallel, [ problem_01_1(), problem_01_2(),
                    problem_02_1(), problem_02_2(),
                    problem_03_1(), problem_03_2(),
                    problem_04_1(), problem_04_2(),
                    problem_05_1(), problem_05_2(),
                    problem_06_1(), problem_06_2(),
                    problem_07_1(), problem_07_2(),
                    problem_09_1(), problem_09_2() ] }.
