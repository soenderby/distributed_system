-module(blocking_handler).

-export([start/1]).

start(Pid) ->
    receive
	Msg ->
	    Pid ! Msg
    end,
    loop().

loop() ->
    timer:sleep(100),
    loop().
