-module(glacier_ffi).

-export([start_file_change_watcher/1, get_cwd_as_binary/0]).

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
            case {lists:member(modified, Changes),
                  string:find(Path, SrcPath) =:= Path,
                  string:find(Path, TestPath) =:= Path}
            of
                {true, true, false} ->
                    % io:format("~p ", [Changes]),
                    Callback(src_module_kind, iolist_to_binary(Path));
                {true, false, true} ->
                    % io:format("~p ", [Changes]),
                    Callback(test_module_kind, iolist_to_binary(Path));
                _ ->
                    % io:format("Not in \"./src\": ~p\n", [{Path, _Changes}]),
                    nil
            end,
            process_file_update_and_loop(SrcPath, TestPath, Callback);
        _Any ->
            % io:format("Unexpected message: ~p\n", [_Any]),
            process_file_update_and_loop(SrcPath, TestPath, Callback)
    end.
