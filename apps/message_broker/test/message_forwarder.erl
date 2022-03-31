-module(message_forwarder).

-export([start/1]).

start(Pid) ->
    loop(Pid).

loop(Pid) ->
    receive 
	Msg ->
	    Pid ! Msg
    end,
    loop(Pid).
