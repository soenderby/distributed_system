-module(mb_log_writer).
-behaviour(gen_server).

-export([
	 start_link/3,
	 stop/1,
	 append/2,
	 get_messages/1,
	 persist/1
	]).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-define(SERVER, ?MODULE).

-record(state, {
		messages = [],
		persist_fun,
		message_limit,
		timeout
	       }).

start_link(Persist_fun, Message_limit, Timeout) ->
    gen_server:start_link(?MODULE, [Persist_fun, Message_limit, Timeout], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

append(Pid, Messages) ->
    gen_server:cast(Pid, {append, Messages}),
    ok.

get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

persist(Pid) ->
    gen_server:cast(Pid, persist),
    ok.

%%% Callbacks
init([Persist_fun, Message_limit, Timeout]) ->
    process_flag(trap_exit, true),
    {ok, #state{
	    persist_fun = Persist_fun,
	    message_limit = Message_limit,
	    timeout = Timeout
	   }}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_messages, _From, State = #state{messages = Messages, timeout = Timeout}) ->
    Reply = Messages,
    {reply, Reply, State, Timeout}.

handle_cast({append, New_messages}, 
	    State = #state{
		       messages = Messages, 
		       message_limit = Message_limit,
		       persist_fun = Persist_fun,
		       timeout = Timeout
		      }) ->
    Combined_messages = New_messages ++ Messages,
    Total_length = length(Combined_messages),
    case Total_length >= Message_limit of
	true -> 
	    Keep_count = Total_length - Message_limit,
	    Keep_messages = lists:sublist(Combined_messages, Keep_count),
	    Persist_messages = lists:nthtail(Keep_count, Combined_messages),
	    Persist_fun(Persist_messages);
	false -> 
	    Keep_messages = Combined_messages
    end,
    New_state = State#state{messages = Keep_messages},
    {noreply, New_state, Timeout};
handle_cast(persist, State = #state{persist_fun = Persist_fun, messages = Messages}) ->
    Persist_fun(Messages),
    New_state = State#state{messages = []},
    {noreply, New_state}.

handle_info(timeout, State = #state{messages = Messages, persist_fun = Persist_fun}) ->
    Persist_fun(Messages),
    {noreply, State#state{messages = []}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
