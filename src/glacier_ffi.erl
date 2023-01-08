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
        {_Pid, {fs, file_event}, {FilePath, Changes}} ->
			% By stripping spaces we just do not support them at all in source
			% file names. This makes handling across targets and in regards do
			% module names and file name mapping a lot easier:
			FilePathNoSpaces = re:replace(FilePath, "\\s+", "", [global,{return,list}]),
			% Detect the parent dir type:
            CwdBinary = get_cwd_as_binary(),
            SrcDir = <<CwdBinary/binary, "/src">>,
            TestDir = <<CwdBinary/binary, "/test">>,
            IsSrcModuleKind = string:find(FilePath, SrcDir) =:= FilePathNoSpaces,
            IsTestModuleKind = string:find(FilePathNoSpaces, TestDir) =:= FilePathNoSpaces,
			% Make sure files are actually existing and end with .gleam:
            FileExists = filelib:is_regular(FilePathNoSpaces),
            IsGleamFile = gleam_stdlib:string_ends_with(list_to_bitstring(FilePathNoSpaces), <<".gleam"/utf8>>),
            MatchingEvent =
                lists:member(modified, Changes)
                orelse lists:member(created, Changes)
                orelse lists:member(renamed, Changes),
            case {FileExists, IsGleamFile, MatchingEvent, IsSrcModuleKind, IsTestModuleKind} of
                {true, true, true, true, _} ->
                    % io:format("\n~p: ~p\n\n", [Changes, FilePathNoSpaces]),
                    FileChangeHandlerFn([{src_module_kind, iolist_to_binary(FilePathNoSpaces)}]);
                {true, true, true, _, true} ->
                    % io:format("\n~p: ~p \n\n", [Changes, FilePathNoSpaces]),
                    FileChangeHandlerFn([{test_module_kind, iolist_to_binary(FilePathNoSpaces)}]);
                {_, _, _, _, _} ->
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
