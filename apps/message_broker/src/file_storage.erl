-module(file_storage).
-behaviour(gen_server).

-export([
	 start_link/2,
	 stop/1,
	 get_messages/3,
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

-record(state, {messages = [],
	        dir,
		filesize
	       }).

start_link(Path, FileSize) ->
    gen_server:start_link(?MODULE, [Path, FileSize], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

get_messages(Pid, Offset, Amount) ->
    gen_server:call(Pid, {get_messages, Offset, Amount}).

persist(Pid) ->
    gen_server:call(Pid, persist).

%%% Callbacks
init([Path, FileSize]) ->
    ok = filelib:ensure_dir(Path),
    {ok, #state{dir = Path, filesize = FileSize}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({get_messages, Offset, Amount}, _From, State = #state{messages = Msg}) ->
    {reply, lists:sublist(lists:nthtail(Offset,lists:reverse(Msg)), Amount), State};
handle_call(persist, _From, State = #state{messages = Msg, dir = Dir}) ->
    FileName = filename:join(Dir, integer_to_list(length(Msg))),
    ok = file:write_file(FileName, term_to_binary(Msg)),
    {reply, FileName, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State = #state{messages = Messages, dir = Dir, filesize = Size}) ->
    io:format("Received Message: ~p ~n", [Msg]),
    NewMessages = [Msg | Messages],
    Binary = term_to_binary(lists:reverse(NewMessages)),
    case byte_size(Binary) >= Size of 
	true ->
	    FileName = filename:join(Dir, integer_to_list(length(NewMessages))),
	    ok = file:write_file(FileName, Binary),
	    ResList = [];
	false ->
	    ResList = NewMessages
    end,
    {noreply, State#state{messages = ResList}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

