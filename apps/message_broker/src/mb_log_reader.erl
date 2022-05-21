-module(mb_log_reader).

-export([
	 read/4
	]).

read(Keys, Get_value_fun, Start_offset, Amount) ->
    Final_index = Start_offset + (Amount - 1),
    case index_available(Final_index, Keys) of
	true -> 
	    Relevant_keys = keys_for_range(Start_offset, Final_index, Keys),
	    lists:flatten(lists:reverse([Get_value_fun(Key) || Key <- Relevant_keys]));
	false -> 
	    not_available
    end.

index_available(_Key, []) ->
    false;
index_available(Key, Key_list) ->
    % The greatest key indicates the latest message available
    % If the requested key is greater, then the message is not yet available
    not (lists:max(Key_list) < Key).

keys_for_range(First, First, Keys) ->
    case lists:member(First, Keys) of
	true -> [First];
	false -> [first_greater_value(First, Keys)]
    end;
keys_for_range(First, Last, Keys) ->
    Key_for_last_message = first_greater_value(Last, Keys),
    [Key || Key <- Keys, ((Key >= First) and (Key =< Key_for_last_message))].

first_greater_value(_, []) ->
    none;
first_greater_value(Val, [H|_]) when H > Val ->
    H;
first_greater_value(Val, [_|T]) ->
    first_greater_value(Val, T).
