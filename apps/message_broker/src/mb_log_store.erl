-module(mb_log_store).

-behaviour(gen_server).

-export([
	 start_link/1,
	 stop/1,
	 write/2,
	 read/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
		keys_fun,
		get_fun,
		put_fun,
		writer_pid
	       }).

-spec start_link({fun(() -> list(integer())), fun((integer()) -> list()), fun((integer(), list()) -> atom())}) -> ok.
start_link(Callbacks) ->
    gen_server:start_link(?MODULE, [Callbacks], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

write(Pid, Messages) ->
    gen_server:call(Pid, {write, Messages}).

read(Pid, Start_offset, Amount) ->
    gen_server:call(Pid, {read, Start_offset, Amount}).

%%% Callbacks
init([{Fetch_keys_fun, Get_fun, Put_fun}]) ->
    process_flag(trap_exit, true),
    {ok, Writer_pid} = mb_log_writer:start_link(store_messages(Fetch_keys_fun, Put_fun), 5, 50),
    {ok, #state{keys_fun = Fetch_keys_fun, get_fun = Get_fun, put_fun = Put_fun, writer_pid = Writer_pid}}.

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
