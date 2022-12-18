-module(glacier_ffi).

-export([stdout_println/1, list_map/2, string_replace/3, identity/1]).

stdout_println(Binary) ->
    io:put_chars(Binary),
    io:put_chars("\n").

list_map(List, Callable) ->
    lists:map(Callable, List).

string_replace(String, SearchPattern, Replacement) ->
    iolist_to_binary(string:replace(iolist_to_binary(String),
                                    iolist_to_binary(SearchPattern),
                                    iolist_to_binary(Replacement),
                                    all)).

identity(X) ->
    X.
