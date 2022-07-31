-module(file_kv_store).
-behaviour(gen_server).

-export([
	 start_link/1,
	 stop/1,
	 keys/1,
	 get/2,
	 put/3,
	 get_directory/1
	]).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-record(state, {dir}).

start_link(Directory) ->
    gen_server:start_link(?MODULE, [Directory], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

keys(Pid) ->
    gen_server:call(Pid, keys).

get(Pid, Key) ->
    case gen_server:call(Pid, {get, Key}) of 
	key_not_found ->
	    throw({key_not_found, Key});
	Keys -> Keys
    end.

put(Pid, Key, Value) ->
    gen_server:cast(Pid, {put, Key, Value}),
    ok.

get_directory(Pid) ->
    gen_server:call(Pid, get_directory).

%%% Callbacks
init([Directory]) ->
    case file:make_dir(Directory) of 
	ok -> {ok, #state{dir = Directory}};
	{error, eexist} -> {ok, #state{dir = Directory}};
	Error -> Error
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(keys, _From, State=#state{dir = Dir}) ->
    Keys = read_filenames_in_directory(Dir),
    {reply, Keys, State};
handle_call({get, Key}, _From, State=#state{dir = Dir}) ->
    % Here keys are retrieved when they may not have changed. They should be cached.
    Keys = read_filenames_in_directory(Dir),
    case lists:member(Key, Keys) of
	true ->
	    Value = read_values_from_file(Dir, Key),
	    {reply, Value, State};
	false ->
	    {reply, key_not_found, State}
    end;
handle_call(get_directory, _From, State=#state{dir = Dir}) ->
    {reply, Dir, State}.

handle_cast({put, Key, Value}, State=#state{dir = Dir}) ->
    create_file_with_content(Dir, Key, Value),
    {noreply, State}.

handle_info(_Info, State) ->
    {norely, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions
read_values_from_file(Directory, Filename) ->
    Filepath = get_file_path(Directory, Filename),
    {ok, Binary} = file:read_file(Filepath),
    binary_to_term(Binary).

create_file_with_content(Directory, Filename, Content) ->
    Filepath = get_file_path(Directory, Filename),
    Binary = term_to_binary(Content),
    file:write_file(Filepath, Binary).

get_file_path(Directory, Filename) when is_integer(Filename) ->
    get_file_path(Directory, integer_to_list(Filename));
get_file_path(Directory, Filename) ->
    filename:join(Directory, Filename).

read_filenames_in_directory(Directory) ->
    {ok, ListOfFiles} = file:list_dir(Directory),
    [decode_filename(FilenameString) || FilenameString <- ListOfFiles].

% I do not like a catch to be part of the 'happy path'
% However not sure how else to determine if an atom is, or can be, an integer
decode_filename(Filename) ->
    try list_to_integer(Filename)
    catch
	error:badarg -> list_to_atom(Filename)
    end.
