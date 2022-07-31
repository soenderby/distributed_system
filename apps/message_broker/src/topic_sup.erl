-module(topic_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Name) ->
    supervisor:start_link(?MODULE, [Name]).

init([Name]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime},
	  [{server,
	    {topic_server, start_link, [Name, self()]},
	    permanent,
	    10500, % Shutdown time
	    worker,
	    [topic_server]}]}}.
