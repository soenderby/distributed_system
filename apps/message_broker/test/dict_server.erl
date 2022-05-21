-module(dict_server).

-export([
	 start/0,
	 stop/0,
	 keys/0,
	 get/1,
	 put/2
	]).

start() ->
    Pid = spawn(fun() ->  loop(dict:new()) end),
    register(dict_server, Pid).

stop() ->
    exit(whereis(dict_server), kill).

keys() ->
    dict_server ! {self(), keys},
    receive 
	Keys -> Keys
    after 100 ->
	    failed_to_get_keys
    end.

get(Key) ->
    dict_server ! {self(), get, Key},
    receive 
	Value -> Value
    after 100 ->
	    failed_to_get_value
    end.

put(Key, Value) ->
    dict_server ! {put, Key, Value},
    ok.


%%% Internal functions
loop(Dict) ->
    receive
	{From, keys} ->
	    From ! dict:fetch_keys(Dict),
	    loop(Dict);
	{From, get, Key} ->
	    From ! dict:fetch(Key, Dict),
	    loop(Dict);
	{put, Key, Value} ->
	    New_dict = dict:store(Key, Value, Dict),
	    loop(New_dict)
    end.



