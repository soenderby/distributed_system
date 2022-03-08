-module(worker_pool).
-behaviour(application).

-export([
	 start/2,
	 stop/1
	]).

-export([
	 start_pool/3,
	 run/2, 
	 sync_queue/2,
	 async_queue/2,
	 stop_pool/1
	]).
 
start(normal, _Args) ->
    worker_pool_top_sup:start_link().
 
stop(_State) ->
    ok.
 
start_pool(Name, Limit, {M,F,A}) ->
    worker_pool_top_sup:start_pool(Name, Limit, {M,F,A}).
 
stop_pool(Name) ->
    worker_pool_top_sup:stop_pool(Name).
 
run(Name, Args) ->
    worker_pool_server:run(Name, Args).
 
async_queue(Name, Args) ->
    worker_pool_server:async_queue(Name, Args).
 
sync_queue(Name, Args) ->
    worker_pool_server:sync_queue(Name, Args).
