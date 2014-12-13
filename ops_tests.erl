-module(ops_tests).
-include_lib("eunit/include/eunit.hrl").
 
add_a_value_test() ->
4 = ops:add_a_value(2,2).