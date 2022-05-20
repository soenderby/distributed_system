-module(mb_log_store).

-behaviour(gen_server).

-export([
	 start_link/0,
	 stop/1,
	 write/2,
	 read/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
		messages = []
	       }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

write(Pid, Messages) ->
    gen_server:call(Pid, {write, Messages}).

read(Pid, Start_offset, Amount) ->
    gen_server:call(Pid, {read, Start_offset, Amount}).

%%% Callbacks
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({write, Messages}, _From, State) ->
    {reply, ok, State#state{messages = Messages}};
handle_call({read, _Start_offset, _Amount}, {Pid, _}, State = #state{messages = Messages}) ->
    Pid ! Messages,
    {reply, ok, State}.

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
