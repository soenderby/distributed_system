-module(worker_pool_sup).

-behaviour(supervisor).

-export([start_link/3, init/1]).

-define(SERVER, ?MODULE).

start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_all, MaxRestart, MaxTime},
	  [{serv,
	    {worker_pool_server, start_link, [Name, Limit, self(), MFA]},
	    permanent,
	    5000, % Shutdown time
	    worker,
	    [worker_pool_server]}]}}.
