-module(mb_top_sup).
-behaviour(supervisor).
-export([start_link/0, create_topic/1, remove_topic/1]).
-export([init/1]).
 
start_link() ->
    supervisor:start_link({local, message_broker}, ?MODULE, []).

init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

create_topic(Name) ->
    ChildSpec = {Name,
		 {topic_server, start_link, [Name]},
		 permanent, 10500, worker, [topic_server]},
    supervisor:start_child(message_broker, ChildSpec).

remove_topic(Name) ->
    supervisor:terminate_child(message_broker, Name),
    supervisor:delete_child(message_broker, Name).
