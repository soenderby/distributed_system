-module(worker_pool_top_sup).

-behaviour(supervisor).

%% API
-export([
	 start_link/0,
	 start_pool/3,
	 stop_pool/1
	]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, worker_pool).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
		 {worker_pool_sup, start_link, [Name, Limit, MFA]},
		 permanent, 10500, supervisor, [ppool_sup]},
    supervisor:start_child(worker_pool, ChildSpec).    

stop_pool(Name) ->
    supervisor:terminate_child(worker_pool, Name),
    supervisor:delete_child(worker_pool, Name).

init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.
