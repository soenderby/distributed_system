-module(mb_log_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(Name, Callbacks) ->
    supervisor:start_link(?MODULE, [Name, Callbacks]).

init([Name, Callbacks]) ->
    SupFlags = #{strategy => one_for_all,
                intensity => 6,
                period => 3600,
                auto_shutdown => any_significant},
    ChildSpec = #{id => mb_log_store,
		  start => {mb_log_store, start_link, [Name, Callbacks, self()]},
		  restart => transient,
		  significant => true,
		  shutdown => brutal_kill,
		  type => worker,
		  modules => [mb_log_store]},
   % ChildSpec = {mb_log_store,
%		 {mb_log_store, start_link, [Name, Callbacks, self()]},
%		 permanent, 10500, worker, [mb_log_store]},
    {ok, {SupFlags, [ChildSpec]}}.
