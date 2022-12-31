-module(glacier_ffi).

-export([start_file_change_watcher/1, get_cwd_as_binary/0, find_files_recursive/2]).

get_cwd() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

get_cwd_as_binary() ->
    iolist_to_binary(get_cwd()).

start_file_change_watcher(FileChangeHandlerFn) ->
    watch_directory(fs_src_watcher, src_module_kind, FileChangeHandlerFn),
    % watch_directory(fs_test_watcher, test_module_kind, FileChangeHandlerFn),
    timer:sleep(infinity),
    nil.

process_file_update_and_loop(ModuleKind, WatchPath, FileChangeHandlerFn) ->
    receive
        {_Pid, {fs, file_event}, {Path, Changes}} ->
            CwdBinary = get_cwd_as_binary(),
            SrcDir = <<CwdBinary/binary, "/src">>,
            TestDir = <<CwdBinary/binary, "/test">>,
            IsSrcModuleKind = string:find(Path, SrcDir) =:= Path,
            IsTestModuleKind = string:find(Path, TestDir) =:= Path,
            MatchingEvent =
                lists:member(modified, Changes)
                orelse lists:member(created, Changes)
                orelse lists:member(renamed, Changes),
            case {MatchingEvent, IsSrcModuleKind, IsTestModuleKind} of
                {true, true, _} ->
                    % io:format("~p ", [Path]),
                    FileChangeHandlerFn([{src_module_kind, iolist_to_binary(Path)}]);
                {true, _, true} ->
                    % io:format("~p ", [Path]),
                    FileChangeHandlerFn([{test_module_kind, iolist_to_binary(Path)}]);
                {_, _, _} ->
                    nil
            end,
            process_file_update_and_loop(ModuleKind, WatchPath, FileChangeHandlerFn);
        _Any ->
            process_file_update_and_loop(ModuleKind, WatchPath, FileChangeHandlerFn)
    end.

watch_directory(WatcherAtom, ModuleKind, FileChangeHandlerFn) ->
    Cwd = get_cwd(),
    WatchPath = Cwd,
    % SubPath = case ModuleKind of
    %     src_module_kind -> "src";
    %     test_module_kind -> "test"
    % end,
    % WatchPath = filename:join([Cwd, SubPath]),
    % io:format("~p ", [WatchPath]),
    fs:start_link(WatcherAtom, WatchPath),
    fs:subscribe(WatcherAtom),
    process_file_update_and_loop(ModuleKind, WatchPath, FileChangeHandlerFn),
    nil.

% Derived from gleeunit_ffi.erl
find_files_recursive(In, Pattern) ->
    Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
    lists:map(fun list_to_binary/1, Results).
