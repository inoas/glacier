-module(glacier_ffi).

-export([start_watcher/1]).

start_watcher(Callback) ->
    {ok, Cwd} = file:get_cwd(),
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
					reload_all_available_modules(),
                    Callback(in_src_path, iolist_to_binary(Path));
                {true, false, true} ->
                    % io:format("~p ", [Changes]),
                    Callback(in_test_path, iolist_to_binary(Path));
                _ ->
                    % io:format("Not in \"./src\": ~p\n", [{Path, _Changes}]),
                    nil
            end,
            process_file_update_and_loop(SrcPath, TestPath, Callback);
        _Any ->
            % io:format("Unexpected message: ~p\n", [_Any]),
            process_file_update_and_loop(SrcPath, TestPath, Callback)
    end.

reload_all_available_modules() ->
	AllAvailableModules = code:all_available(),
	io:format("All available modules: ~p\n", [AllAvailableModules]).
