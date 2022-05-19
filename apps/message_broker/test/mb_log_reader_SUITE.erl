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
     message_is_last_in_segment
    ].

first_message_not_available(_Config) -> 
    not_available = mb_log_reader:read([1,5,9], fun noop/1, 11, 1).

last_message_not_available(_Config) ->
    not_available = mb_log_reader:read([1,5,9], fun noop/1, 8, 4).

message_available_in_last_segment(_Config) ->
    ok = mb_log_reader:read([1,5,9], send_to(self()), 6, 1),
    [9] = flush_messages().
	
message_available_in_middle_segment(_Config) ->
    ok = mb_log_reader:read([1,5,9], send_to(self()), 4, 1),
    [5] = flush_messages().

messages_across_multiple_segments(_Config) ->
    ok = mb_log_reader:read([1,5,9], send_to(self()), 4, 3),
    [5,9] = flush_messages(). 

message_is_last_in_segment(_Config) ->
    ok = mb_log_reader:read([1,5,9], send_to(self()), 5, 1),
    [5] = flush_messages().

%%% Internal functions
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
