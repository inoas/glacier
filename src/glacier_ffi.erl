-module(glacier_ffi).

-export([start_file_change_watcher/1, get_cwd_as_binary/0, find_files_recursive/2]).

get_cwd() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

get_cwd_as_binary() ->
    iolist_to_binary(get_cwd()).

start_file_change_watcher(Callback) ->
    Cwd = get_cwd(),
    fs:start_link(fs_watcher, Cwd),
    fs:subscribe(fs_watcher),
    % fs:start_looper(),
    SrcPath = string:concat(Cwd, "/src"),
    TestPath = string:concat(Cwd, "/test"),
    process_file_update_and_loop(SrcPath, TestPath, Callback),
    timer:sleep(infinity),
    nil.

process_file_update_and_loop(SrcPath, TestPath, Callback) ->
    receive
        {_Pid, {fs, file_event}, {Path, Changes}} ->
            case lists:member(modified, Changes)
            of
                true ->
                    % io:format("~p ", [Changes]),
                    Callback(iolist_to_binary(Path));
                false ->
                    % io:format("Not in \"./src\": ~p\n", [{Path, _Changes}]),
                    nil
            end,
            process_file_update_and_loop(SrcPath, TestPath, Callback);
        _Any ->
            % io:format("Unexpected message: ~p\n", [_Any]),
            process_file_update_and_loop(SrcPath, TestPath, Callback)
    end.

% Derived from gleeunit_ffi.erl
find_files_recursive(Pattern, In) ->
    Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
    lists:map(fun list_to_binary/1, Results).
