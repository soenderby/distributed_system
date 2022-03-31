-module(mb).

-export([
	 create_topic/1,
	 remove_topic/1,
	 get_subscribers/1,
	 subscribe/3,
	 remove_subscriber/2,
	 message/2
	]).

create_topic(Name) ->
    mb_top_sup:create_topic(Name).

remove_topic(Name) ->
    mb_top_sup:remove_topic(Name).

get_subscribers(Topic) ->
    topic_server:get_subscribers(Topic).

subscribe(Topic, SubName, Pid) ->
    topic_server:subscribe(Topic, SubName, Pid).

remove_subscriber(Topic, SubName) ->
    topic_server:remove_subscriber(Topic, SubName).

message(Topic, Message) ->
    topic_server:message(Topic, Message).
