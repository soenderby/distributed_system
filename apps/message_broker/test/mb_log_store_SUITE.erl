-module(mb_log_store_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(value(Key,Config), proplists:get_value(Key,Config)).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    dict_server:start(),
    {ok, _Sup_pid} = mb_log_sup:start_link(test, {fun dict_server:keys/0, fun dict_server:get/1, fun dict_server:put/2}),
    Pid = whereis(test),
    [{pid, Pid}|Config].

end_per_testcase(_TestCase, _Config) ->
    dict_server:stop(),
    ok.

groups() ->
    [].

all() -> 
    [
     start_and_stop,
     read_all_written_messages,
     read_subset_of_messages,
     multiple_segments,
     uses_given_callbacks,
     read_interval_across_multiple_segments
    ].

start_and_stop(_Config) ->
    "Check that the supervisor starts the process, and that it terminates when the supervisor terminates",
    {ok, Sup_pid} = mb_log_sup:start_link(start_stop_test, {fun dict_server:keys/0, fun dict_server:get/1, fun dict_server:put/2}),
    % Unlink so the test process does not terminate when the supervisor does
    true = unlink(Sup_pid),
    % Monitor so message is received when supervisor exits
    Ref = monitor(process, Sup_pid),
    Pid = whereis(start_stop_test),
    true = is_process_alive(Pid),
    exit(Sup_pid, shutdown),
    receive
	{'DOWN', Ref, process, Sup_pid, _Reason} ->
	    false = is_process_alive(Pid)
    after 1000 ->
	    error(exit_timeout)
    end.

read_all_written_messages(Config) ->
    Pid = ?value(pid, Config),
    ok = mb_log_store:write(Pid, [1,2,3,4,5]),
    ok = mb_log_store:write(Pid, [6]),
    [1,2,3,4,5] = mb_log_store:read(Pid, 1, 5).

read_subset_of_messages(Config) ->
    Pid = ?value(pid, Config),
    ok = mb_log_store:write(Pid, [1,2,3,4,5]),
    ok = mb_log_store:write(Pid, [6]),
    [1,2,3,4,5] = mb_log_store:read(Pid, 1,5),
    [2,3,4] = mb_log_store:read(Pid, 2, 3).

multiple_segments(Config) ->
    Pid = ?value(pid, Config),
    ok = mb_log_store:write(Pid, [1,2,3,4,5]),
    ok = mb_log_store:write(Pid, [6,7,8,9,10]),
    ok = mb_log_store:write(Pid, [12]),
    [6,7,8,9,10] = mb_log_store:read(Pid, 6, 5),
    [1,2,3,4,5] = mb_log_store:read(Pid, 1, 5).

uses_given_callbacks(Config) ->
    Pid = ?value(pid, Config),
    dict_server:put(5, [1,2,3,4,5]),
    [1,2,3,4,5] = mb_log_store:read(Pid, 1, 5),
    [2,3,4] = mb_log_store:read(Pid, 2, 3).

read_interval_across_multiple_segments(Config) ->
    Pid = ?value(pid, Config),
    ok = mb_log_store:write(Pid, [1,2,3,4,5]),
    ok = mb_log_store:write(Pid, [6,7,8,9,10]),
    ok = mb_log_store:write(Pid, [12]),
    [3,4,5,6,7] = mb_log_store:read(Pid, 3, 5).

%% Internal functions
start() ->
    start({fun dict_server:keys/0, fun dict_server:get/1, fun dict_server:put/2}).
   
start(Callbacks) ->
    {ok, Pid} = mb_log_store:start_link(Callbacks),
    Pid.

flush_messages() ->
    receive
        X -> [X|flush_messages()]
    after 100 ->
        []
    end.

noop(_) ->
    ok.

noop(_,_) ->
    ok.
