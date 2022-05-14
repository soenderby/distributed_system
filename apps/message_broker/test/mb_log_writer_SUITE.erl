-module(mb_log_writer_SUITE).

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
     append_messages,
     persist_clears_messages,
     persist_calls_callback_with_messages,
     messages_persisted_after_limit,
     messages_persisted_after_time_from_last_message
    ].

start_and_stop(_Config) -> 
    Pid = start(),
    true = is_process_alive(Pid),
    ok = mb_log_writer:stop(Pid),
    false = is_process_alive(Pid).

append_messages(_Config) ->
    Pid = start(),
    First_messages = [1,2],
    ok = mb_log_writer:append(Pid, First_messages),
    First_messages = mb_log_writer:get_messages(Pid),
    Additional_messages = [3,4,5], 
    ok = mb_log_writer:append(Pid, Additional_messages),
    [3,4,5,1,2] = mb_log_writer:get_messages(Pid),
    ok = mb_log_writer:stop(Pid).

persist_clears_messages(_Config) ->
    Pid = start(),
    ok = mb_log_writer:append(Pid, [1,2,3,4,5]),
    true = length(mb_log_writer:get_messages(Pid)) > 0,
    ok = mb_log_writer:persist(Pid),
    true = length(mb_log_writer:get_messages(Pid)) =:= 0,
    ok = mb_log_writer:stop(Pid).

persist_calls_callback_with_messages(_Config) ->
    Pid = start(mock_persist(self())),
    ok = mb_log_writer:append(Pid, [1,2,3]),
    ok = mb_log_writer:persist(Pid),
    [[1,2,3]] = flush_messages(),
    ok = mb_log_writer:stop(Pid).

messages_persisted_after_limit(_Config) ->
    Pid = start(mock_persist(self()), 4),
    ok = mb_log_writer:append(Pid, [1,2]),
    [] = flush_messages(),
    ok = mb_log_writer:append(Pid, [3,4]),
    [[3,4,1,2]] = flush_messages(),
    ok = mb_log_writer:stop(Pid).

messages_persisted_after_time_from_last_message(_Config) ->
    Pid = start(mock_persist(self()), 5, 400),
    ok = mb_log_writer:append(Pid, [1,2,3,4]),
    %% Neither message nor timelimit reached, so no messages should be persisted
    [] = flush_messages(),
    %% wait until timeout is exceeded
    timer:sleep(500),
    [[1,2,3,4]] = flush_messages(),
    ok = mb_log_writer:stop(Pid).

%%% Internal functions
noop(_) ->
    ok.

start() ->
    start(fun noop/1).
start(Fun) ->
    start(Fun, 10).
start(Fun, Message_limit) ->
    start(Fun, Message_limit, 1000).
start(Fun, Message_limit, Timeout) ->
    {ok, Pid} = mb_log_writer:start_link(Fun, Message_limit, Timeout),
    Pid.

mock_persist(Pid) ->
    fun(Messages) ->
	    Pid ! Messages
    end.

flush_messages() ->
    receive
        X -> [X|flush_messages()]
    after 100 ->
        []
    end.
