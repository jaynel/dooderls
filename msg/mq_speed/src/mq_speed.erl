-module(mq_speed).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    mq_speed_sup:start_link().

start(_StartType, _StartArgs) ->
    mq_speed_sup:start_link().

stop(_State) ->
    ok.
