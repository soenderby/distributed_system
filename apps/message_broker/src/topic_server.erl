-module(topic_server).
-behaviour(gen_server).


-export([
	 start_link/1, 
	 stop/1,
	 subscribe/3,
	 remove_subscriber/2,
	 get_subscribers/1,
	 message/2
	]).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).


-record(state, {name,
		subscribers = [],
		event_manager}).

start_link(Name) ->
   gen_server:start_link({local, Name}, ?MODULE, [Name], []).

stop(Name) ->
    gen_server:call(Name, stop).

subscribe(Topic, SubName, Pid) ->
    gen_server:call(Topic, {add_subscriber, SubName, Pid}).

remove_subscriber(Topic, SubName) ->
    gen_server:call(Topic, {remove_subscriber, SubName}).

get_subscribers(Topic) ->
    gen_server:call(Topic, get_subscribers).

message(Topic, Message) ->
    gen_server:call(Topic, {message, Message}).

init([Name]) ->
    {ok, Pid} = gen_event:start_link(),
    % Consider if event manager should be linked/monitored
    {ok, #state{name = Name, event_manager = Pid}}.


handle_call(
  {add_subscriber, SubName, ReceiverPid}, _From,
  State = #state{event_manager = Pid, subscribers = Subs}) ->

    ok = gen_event:add_handler(Pid, {event_forwarder, SubName}, ReceiverPid),
    % Monitor the subscriber, so it can be removed if it crashes
    erlang:monitor(process, ReceiverPid),
    {reply, ok, State#state{subscribers = [{SubName, ReceiverPid} | Subs]}};

handle_call({remove_subscriber, SubName}, _From,
	   State = #state{event_manager = Pid, subscribers = Subs}) ->
    {reply, ok, State#state{
		  subscribers = remove_subscriber(Pid, SubName, Subs)}};

handle_call(get_subscribers, _From, State = #state{subscribers = Subs}) ->
    {reply, Subs, State};

handle_call({message, Message}, _From, State = #state{event_manager = Pid}) ->
    gen_event:notify(Pid, Message),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason},
	    State = #state{event_manager = Event_Man, subscribers = Subs}) ->
    %% Remove terminated subscribers
    {noreply, State#state{subscribers = remove_subscriber(Event_Man, Pid, Subs)}};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State = #state{event_manager = Pid}) ->
    gen_event:stop(Pid).

%%% Internal functions
remove_subscriber(Event_Man, Pid, Subscribers) when is_pid(Pid) ->
    % Find subscriber name from the Pid
    {SubName, Pid} = lists:keyfind(Pid, 2, Subscribers),
   remove_subscriber(Event_Man, SubName, Subscribers);
remove_subscriber(Event_Man, SubName, Subscribers) ->
    % Remove the event handler from the event manager
    gen_event:delete_handler(Event_Man, {event_forwarder, SubName}, unsubscribe),
    % Remove the subscriber from the list of subscribers, and return the updated list
    proplists:delete(SubName, Subscribers).
