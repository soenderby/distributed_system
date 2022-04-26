-module(file_storage_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Config) ->
    Config.

end_per_group(_Config) ->
    ok.
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [
     start_and_stop,
     receives_messages_and_makes_them_available,
     multiple_reads_with_different_offsets,
     persists_messages_to_files_at_given_path,
     persists_messages_to_files_when_size_exceeded
    ].

start_and_stop(Config) ->
    {ok, Pid} = file_storage:start_link(test_dir(Config), test_size()),
    true = is_process_alive(Pid),
    ok = file_storage:stop(Pid),
    false = is_process_alive(Pid).

receives_messages_and_makes_them_available(Config) ->
    {ok, Pid} = file_storage:start_link(test_dir(Config), test_size()),
    Pid ! [{test, message}],
    [[{test, message}]] = file_storage:get_messages(Pid, 0, 1),
    ok = file_storage:stop(Pid).
    
multiple_reads_with_different_offsets(Config) ->
    %% Users of file_storage should be responsible for maintaining their position
    %% Multiple users should be able to read from different places in the log
    {ok, Pid} = file_storage:start_link(test_dir(Config), test_size()),
    Pid ! [{message, one}],
    Pid ! [{message, two}],
    Pid ! [{message, three}],
    Pid ! [{message, four}],
    [[{message, three}], [{message, four}]] = file_storage:get_messages(Pid, 2, 2),
    [[{message, one}], [{message, two}]] = file_storage:get_messages(Pid, 0, 2),
    ok = file_storage:stop(Pid).

creates_given_dir_if_not_exists(Config) ->
    Path = test_dir(Config, "non_existing_folder"),
    false = filelib:is_dir(Path),
    {ok, Pid} = file_storage:start_link(Path, test_size()),
    true = filelib:is_dir(Path),
    ok = file_storage:stop(Pid).

persists_messages_to_files_at_given_path(Config) ->
    {ok, Pid} = file_storage:start_link(test_dir(Config), test_size()),
    Pid ! [{test, message}],
    FilePath = file_storage:persist(Pid),
    {ok, Bin} = file:read_file(FilePath),
    [[{test, message}]] = binary_to_term(Bin),
    file_storage:stop(Pid).

persists_messages_to_files_when_size_exceeded(Config) ->
    ok = file:del_dir_r(test_dir(Config)),
    {ok, Pid} = file_storage:start_link(test_dir(Config), 110),
    % size of messages is 131 bytes
    Messages = [
		[{message, one}], [{message, two}], [{message, three}],
		[{message, four}], [{message, five}]
	       ],
    send_messages(Pid, Messages),
    timer:sleep(100),
    {ok, Files} = file:list_dir(test_dir(Config)),
    % There should only be a single file
    io:format("Files: ~p ~n", [Files]),
    {ok, Bin} = file:read_file(filename:join(test_dir(Config), hd(Files))),
    Expected = Messages,
    Content = binary_to_term(Bin),
    Expected = Content.

%% Internal functions 
test_dir(Config) ->
    test_dir(Config, "test_dir").
test_dir(Config, FolderName) ->
    PrivDir = ?config(priv_dir, Config),
    Path = PrivDir ++ FolderName ++ "/",
    Path.

test_size() ->
    110.

send_messages(_Pid, []) ->
    ok;
send_messages(Pid, [H|T]) ->
    Pid ! H,
    send_messages(Pid, T).

