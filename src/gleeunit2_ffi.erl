-module(gleeunit2_ffi).

-export([find_files/2, should_equal/2, should_not_equal/2, should_be_ok/1,
         should_be_error/1, get_cwd/0, get_cwd_as_binary/0]).

-include_lib("eunit/include/eunit.hrl").

find_files(Pattern, In) ->
<<<<<<< HEAD
<<<<<<< HEAD
  Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
  lists:map(fun list_to_binary/1, Results).

<<<<<<< HEAD

should_equal(Actual, Expected) -> 
    ?assertEqual(Expected, Actual),
    nil.
should_not_equal(Actual, Expected) -> 
    ?assertNotEqual(Expected, Actual),
    nil.
should_be_ok(A) -> 
    ?assertMatch({ok, _}, A),
    element(2, A).
should_be_error(A) -> 
=======
    Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
    lists:map(fun list_to_binary/1, Results).
=======
  Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
  lists:map(fun list_to_binary/1, Results).

>>>>>>> c36ab77 (polish)

=======
>>>>>>> 0244ef2 (move testing logic into gleam)
should_equal(Actual, Expected) ->
    ?assertEqual(Expected, Actual),
    nil.

should_not_equal(Actual, Expected) ->
    ?assertNotEqual(Expected, Actual),
    nil.

should_be_ok(A) ->
    ?assertMatch({ok, _}, A),
    element(2, A).

should_be_error(A) ->
>>>>>>> 52d5260 (integrate gleeunit)
    ?assertMatch({error, _}, A),
    element(2, A).

get_cwd() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

get_cwd_as_binary() ->
    iolist_to_binary(get_cwd()).
