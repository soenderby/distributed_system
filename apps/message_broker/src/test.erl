-module(test).

-export([
	start/0
       ]).

start() ->
    mb:create_topic(test),
    ConsolePid = console_logger:start(),
    mb:subscribe(test, console_log, ConsolePid),
    {ok, Cwd} = file:get_cwd(),
    LogDir = filename:join(Cwd,"logs"),
    {ok, FileStorePid} = file_kv_store:start_link(LogDir),
    LogPid = message_receiver(FileStorePid),
    mb:subscribe(test, file_log, LogPid),
    counter(test, 10),
    FinalContent = file_kv_store:get(FileStorePid, test),
    ConsolePid ! FinalContent,
    mb:remove_topic(test).

message_receiver(Pid) ->
    spawn(fun() -> receiver_loop(Pid) end).

receiver_loop(Pid) ->
    try 
	PrevContent = file_kv_store:get(Pid, test),
	store_message(Pid, PrevContent)
    catch
	throw:_Msg -> store_message(Pid, [])
    end,
    receiver_loop(Pid).

store_message(Pid, PrevContent) ->
    receive
	Msg -> file_kv_store:put(Pid, test, lists:append(PrevContent, Msg))
    end.

counter(Topic, 0) ->
    mb:message(Topic, [{counter, stopped}]);
counter(Topic, Number) ->
    mb:message(Topic, [{counter, Number}]),
    timer:sleep(1000),
    counter(Topic, Number-1).
