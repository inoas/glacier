-module(glacier_ffi).

-export([stdout_println/1, list_map/2, string_replace/3, identity/1, start_watcher/1,
         int_to_string/1]).

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

int_to_string(Int) ->
    integer_to_list(Int).

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
