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
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [
     start_and_stop,
     read_written_messages
    ].

start_and_stop(_Config) -> 
    Pid = start(),
    true = is_process_alive(Pid),
    ok = mb_log_store:stop(Pid),
    false = is_process_alive(Pid).

read_written_messages(_Config) ->
    Pid = start(),
    ok = mb_log_store:write(Pid, [1,2,3,4,5]),
    ok = mb_log_store:read(Pid, 1, 5),
    timer:sleep(100),
    [[1,2,3,4,5]] = flush_messages(),
    mb_log_store:stop(Pid).



%% Internal functions
start() ->
    {ok, Pid} = mb_log_store:start_link(),
    Pid.

flush_messages() ->
    receive
        X -> [X|flush_messages()]
    after 100 ->
        []
    end.
