-module(mb_log_store_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

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
    Config.

end_per_testcase(_TestCase, _Config) ->
   dict_server:stop(),
    ok.

%% TODO: Create a group for tests that use the process dictionary and one for those that use the dict_server
groups() ->
    [].

all() -> 
    [
     start_and_stop,
     read_all_written_messages,
     read_subset_of_messages,
%     multiple_segments,
     uses_given_callbacks
    ].

start_and_stop(_Config) -> 
    Pid = start(),
    true = is_process_alive(Pid),
    ok = mb_log_store:stop(Pid),
    false = is_process_alive(Pid).

read_all_written_messages(_Config) ->
    Pid = start(),
    ok = mb_log_store:write(Pid, [1,2,3,4,5]),
    ok = mb_log_store:write(Pid, [6]),
    [1,2,3,4,5] = mb_log_store:read(Pid, 1, 5),
    mb_log_store:stop(Pid).

read_subset_of_messages(_Config) ->
    Pid = start(),
    ok = mb_log_store:write(Pid, [1,2,3,4,5]),
    ok = mb_log_store:write(Pid, [6]),
    [1,2,3,4,5] = mb_log_store:read(Pid, 1,5),
    [2,3,4] = mb_log_store:read(Pid, 2, 3),
    mb_log_store:stop(Pid).

multiple_segments(_Config) ->
    Pid = start(),
    ok = mb_log_store:write(Pid, [1,2,3,4,5]),
    ok = mb_log_store:write(Pid, [6,7,8,9,10]),
    [1,2,3,4,5] = mb_log_store:read(Pid, 1, 5),
    [6,7,8,9,10] = mb_log_store:read(Pid, 6, 5),
    mb_log_store:stop(Pid).

uses_given_callbacks(_Config) ->
    Pid = start(),
    dict_server:put(5, [1,2,3,4,5]),
    [1,2,3,4,5] = mb_log_store:read(Pid, 1, 5),
    [2,3,4] = mb_log_store:read(Pid, 2, 3),
    mb_log_store:stop(Pid).

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
