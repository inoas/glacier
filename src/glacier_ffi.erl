-module(glacier_ffi).

-export([start_file_change_watcher/1, get_cwd_as_binary/0, find_project_files/2,
         shell_exec/1]).

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

% Derived from gleeunit_ffi.erl
find_project_files(Pattern, In) ->
    Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
    lists:map(fun list_to_binary/1, Results).

% shell_exec(Cmd) when is_binary(Cmd) ->
%     os:cmd().

% https://stackoverflow.com/questions/27028486/how-to-execute-system-command-in-erlang-and-get-results-using-oscmd-1
shell_exec(Command) when is_binary(Command) ->
    Command@2 = binary_to_list(Command),
    Port = open_port({spawn, Command@2}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Sofar | Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    true
            end,
            receive
                {'EXIT', Port, _} ->
                    ok
            after 1 ->              % force context switch
                ok
            end,
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, lists:flatten(Sofar)}
    end.
