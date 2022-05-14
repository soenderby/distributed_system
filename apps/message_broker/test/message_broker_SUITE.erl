-module(message_broker_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:start(message_broker),
    Config.

end_per_suite(_Config) ->
    application:stop(message_broker),
    ok.

init_per_group(with_handler, Config) ->
    %mb:create_topic(test),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(with_handler, _Config) ->
    %mb:remove_topic(test);
    ok;
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %application:start(message_broker),
    mb:create_topic(test),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %application:stop(message_broker),
    mb:remove_topic(test),
    ok.

groups() ->
    [
     {with_handler, [], 
      [
       subscribers_can_be_added_and_removed,
       crashed_subscribers_are_removed,
       subscribers_receive_messages,
       unsubscribed_handlers_do_not_receive_messages,
       blocking_subscriber_should_not_block_messages_to_others
      ]
     }].

all() -> 
    [
     {group, with_handler}
    ].

subscribers_can_be_added_and_removed(_Config) -> 
    [] = mb:get_subscribers(test),
    SubPid = create_forwarder(self()),
    ok = mb:subscribe(test, test_name, SubPid),
    [{test_name, SubPid}] = mb:get_subscribers(test),
    ok = mb:remove_subscriber(test, test_name),
    [] = mb:get_subscribers(test).

crashed_subscribers_are_removed(_Config) ->
    SubPid = create_forwarder(self()),
    ok = mb:subscribe(test, test_name, SubPid),
    [{test_name, SubPid}] = mb:get_subscribers(test),
    exit(SubPid, kill),
    false = is_process_alive(SubPid),
    [] = mb:get_subscribers(test).

subscribers_receive_messages(_Config) ->
    ok = mb:subscribe(test, sub1, create_forwarder(self())),
    % Single subscriber
    ok = mb:message(test, [{message1, test1}]),
    [[{message1, test1}]] = flush_messages(),

    % Multiple subscribers
    ok = mb:subscribe(test, sub2, create_forwarder(self())),
    ok = mb:message(test, [{message2, test2}]),
    [[{message2, test2}],[{message2, test2}]] = flush_messages().

unsubscribed_handlers_do_not_receive_messages(_Config) ->
    ok = mb:subscribe(test, sub1, create_forwarder(self())),
    ok = mb:subscribe(test, sub2, create_forwarder(self())),
    % Send message both subscribers should receive
    mb:message(test, [{message, one}]),
    ok = mb:remove_subscriber(test, sub1),
    % Only the remaining subscriber should receive new messages
    mb:message(test, [{message, two}]),
    [
     [{message, one}], [{message, one}],
     [{message, two}]
    ] = flush_messages().
    
blocking_subscriber_should_not_block_messages_to_others(_Config) ->
    NonBlockingPid = create_forwarder(self()),
    BlockingPid = create_subscriber(blocking_handler, self()),
    mb:subscribe(test, non_blocking, NonBlockingPid),
    % Add subscriber that will forward first message, then loop forever
    mb:subscribe(test, blocking, BlockingPid),
    % Message that will cause subscriber to block
    mb:message(test, [{message, one}]),
    timer:sleep(10),
    % Message that blocked subscriber will not handle
    mb:message(test, [{message, two}]),
    [
     [{message, one}],
     [{message, one}],[{message, two}]
    ] = flush_messages(),
    [{blocking, BlockingPid}, {non_blocking, NonBlockingPid}] = mb:get_subscribers(test).
    
    

%% Helper functions
flush_messages() ->
    receive
        X -> [X|flush_messages()]
    after 100 ->
        []
    end.

create_forwarder(ReceiverPid) ->
    create_subscriber(message_forwarder, ReceiverPid).

create_subscriber(Module, Args) ->
    spawn(Module, start, [Args]).
