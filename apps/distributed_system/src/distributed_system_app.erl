%%%-------------------------------------------------------------------
%% @doc distributed_system public API
%% @end
%%%-------------------------------------------------------------------

-module(distributed_system_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    distributed_system_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
