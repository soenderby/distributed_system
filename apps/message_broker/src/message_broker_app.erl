-module(message_broker_app).
-behaviour(application).

-export([
	 start/2,
	 stop/1
	]).

start(normal, _Args) ->
    mb_top_sup:start_link().

stop(_State) ->
    ok.
