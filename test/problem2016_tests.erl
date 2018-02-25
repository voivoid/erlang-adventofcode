-module( problem2016_tests ).
-include_lib("eunit/include/eunit.hrl").



problem_01_1_test_() -> utils:make_test( "2016_01", fun problem2016_01:solve1/1, 273 ).
