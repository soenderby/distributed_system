-module(mb_log_reader_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [
     first_message_not_available,
     last_message_not_available,
     message_available_in_last_segment,
     message_available_in_middle_segment,
     messages_across_multiple_segments,
     message_is_last_in_segment,
     message_not_available_when_no_key
    ].

first_message_not_available(_Config) -> 
    not_available = mb_log_reader:read([1,5,9], fun get_value/1, 11, 1).

last_message_not_available(_Config) ->
    not_available = mb_log_reader:read([1,5,9], fun get_value/1, 8, 4).

message_available_in_last_segment(_Config) ->
    [9] = mb_log_reader:read([1,5,9], fun get_value/1, 6, 1).
	
message_available_in_middle_segment(_Config) ->
    [5] = mb_log_reader:read([1,5,9], fun get_value/1, 4, 1).

messages_across_multiple_segments(_Config) ->
    [9,5] = mb_log_reader:read([1,5,9], fun get_value/1, 4, 3).

message_is_last_in_segment(_Config) ->
    [5] = mb_log_reader:read([1,5,9], fun get_value/1, 5, 1).

message_not_available_when_no_key(_Config) ->
    not_available = mb_log_reader:read([], fun get_value/1, 1, 1).

%%% Internal functions
get_value(Key) ->
    [Key].

send_to(Pid) ->
    fun(Message) ->
	    Pid ! Message
    end.

flush_messages() ->
    receive
        X -> [X|flush_messages()]
    after 100 ->
        []
    end.

noop(_) ->
    ok.
