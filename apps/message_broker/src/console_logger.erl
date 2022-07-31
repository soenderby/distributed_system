-module(console_logger).

-export([start/0]).

start() ->
    spawn(fun() -> loop() end).

loop() ->
    receive 
	Msg -> io:format("~p~n", [Msg])
    end,
    loop().
