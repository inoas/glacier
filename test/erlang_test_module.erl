-module(erlang_test_module).

-include_lib("eunit/include/eunit.hrl").

one_test() ->
  ?assertEqual(1, 2).

two_test() ->
  2 = 2.
