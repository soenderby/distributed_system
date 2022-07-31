-module(file_kv_store_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_testcase(TestCase, Config) ->
    Test_Dir = filename:join(?config(priv_dir, Config), TestCase),
    [{test_dir, Test_Dir} | Config].

all() ->
    [
     start_and_stop,
     store_and_retrieve_values,
     get_list_of_keys,
     supports_integers_as_keys,
     persists_values_across_restart,
     get_nonexisting_key_throws
    ].

start_and_stop(Config) ->
    TestDir = ?config(test_dir, Config),
    {ok, Pid} = file_kv_store:start_link(TestDir),
    true = is_process_alive(Pid),
    file_kv_store:stop(Pid),
    false = is_process_alive(Pid).

store_and_retrieve_values(Config) ->
    Pid = start_file_kv_store(Config),
    ok = file_kv_store:put(Pid, key, value),
    value = file_kv_store:get(Pid, key),
    file_kv_store:stop(Pid).

get_list_of_keys(Config) ->
    Pid = start_file_kv_store(Config),
    file_kv_store:put(Pid, key1, value),
    file_kv_store:put(Pid, key2, value),
    file_kv_store:put(Pid, key3, value),
    [key1, key2, key3] = file_kv_store:keys(Pid),
    file_kv_store:stop(Pid).

supports_integers_as_keys(Config) ->
    Pid = start_file_kv_store(Config),
    file_kv_store:put(Pid, 123, [{test, value}]),
    [123] = file_kv_store:keys(Pid),
    [{test, value}] = file_kv_store:get(Pid, 123),
    file_kv_store:stop(Pid).

persists_values_across_restart(Config) ->
    Pid1 = start_file_kv_store(Config),
    file_kv_store:put(Pid1, key, value),
    file_kv_store:put(Pid1, 123, [{test, value}]),
    Directory = file_kv_store:get_directory(Pid1),
    file_kv_store:stop(Pid1),

    {ok, Pid2} = file_kv_store:start_link(Directory),
    [123, key] = file_kv_store:keys(Pid2),
    value = file_kv_store:get(Pid2, key),
    [{test, value}] = file_kv_store:get(Pid2, 123),
    file_kv_store:stop(Pid2).

get_nonexisting_key_throws(Config) ->
    Pid = start_file_kv_store(Config),
    Key = no_such_key,
    try
	file_kv_store:get(Pid, Key),
	% This function is not actually available, but it still causes a failure
	ct:fail()
    catch
	throw:Msg ->
	    {key_not_found, Key} = Msg
    end,
    file_kv_store:stop(Pid).

%%% Internal functions
start_file_kv_store(Config) ->
    TestDir = ?config(test_dir, Config),
    {ok, Pid} = file_kv_store:start_link(TestDir),
    Pid.
