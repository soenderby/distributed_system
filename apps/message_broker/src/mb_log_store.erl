-module(mb_log_store).

-behaviour(gen_server).

-export([
	 start_link/3,
	 stop/1,
	 write/2,
	 read/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-type name() :: atom().
-type fetch_keys_fun() :: fun(() -> list(integer())).
-type get_fun() :: fun((integer()) -> list()).
-type put_fun() :: fun((integer(), list()) -> atom()).
-type log_backend_callbacks() :: {fetch_keys_fun(), get_fun(), put_fun()}.
-type supervisor() :: pid().

-define(SERVER, ?MODULE).

-define(SPEC(Persist_fun, Msg_limit, Timeout),
       {mb_log_writer,
        {mb_log_writer, start_link, [Persist_fun, Msg_limit, Timeout]},
        permanent, 10500, worker, [mb_log_writer]}).

-record(state, {
		keys_fun,
		get_fun,
		put_fun,
		writer_pid
	       }).

-spec start_link(name(), log_backend_callbacks(), supervisor()) -> ok.
start_link(Name, Callbacks, Supervisor) ->
    gen_server:start_link({local, Name}, ?MODULE, [Callbacks, Supervisor], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

write(Pid, Messages) ->
    gen_server:call(Pid, {write, Messages}).

read(Pid, Start_offset, Amount) ->
    gen_server:call(Pid, {read, Start_offset, Amount}).

%%% Callbacks
init([{Fetch_keys_fun, Get_fun, Put_fun}, Supervisor]) ->
    process_flag(trap_exit, true),
    self() ! {start_writer, store_messages(Fetch_keys_fun, Put_fun), 5, 50, Supervisor},
    {ok, #state{keys_fun = Fetch_keys_fun, get_fun = Get_fun, put_fun = Put_fun}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({write, Messages}, _From, State = #state{writer_pid = Writer_pid}) ->
    mb_log_writer:append(Writer_pid, Messages),
    {reply, ok, State};
handle_call({read, Start_offset, Amount}, _From, State = #state{keys_fun = Keys_fun, get_fun = Get}) ->
    Segments = mb_log_reader:read(Keys_fun(), get_with_key(Get), Start_offset, Amount),
    {Earliest_segment_end, Earliest_segment_messages} = hd(Segments),
    Earliest_segment_start = (Earliest_segment_end - length(Earliest_segment_messages)),
    Messages = lists:flatten([Msg || {_Key, Msg} <- Segments]),
    Result = list_segment(Messages, Start_offset - Earliest_segment_start, Amount),
    {reply, Result, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({start_writer, Persist_fun, Msg_limit, Timeout, Supervisor}, State) ->
    %{ok, Writer_pid} = mb_log_writer:start_link(store_messages(Fetch_keys_fun, Put_fun), 5, 50),
    {ok, Writer_pid} = supervisor:start_child(Supervisor, ?SPEC(Persist_fun, Msg_limit, Timeout)),
    link(Writer_pid),
    {noreply, State#state{writer_pid = Writer_pid}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%% Internal functions
list_segment(List, Start, End) ->
    lists:sublist(lists:nthtail(Start-1, List), End).

store_messages(Key_fun, Put_fun) ->
    fun(Messages) ->
	    case Key_fun() of
		[] ->
		    New_key = length(Messages);
		Keys ->
		    New_key = lists:max(Keys) + length(Messages)
	    end,
	    Put_fun(New_key, Messages)
    end.

get_with_key(Fun) ->
    fun(Key) ->
	    {Key, Fun(Key)}
    end.
