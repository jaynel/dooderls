-module(mq_speed).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start() -> {ok, pid()} | {error, any()} | ignore.
-spec start(any(), any()) -> {ok, pid()} | {error, any()} | ignore.
-spec stop([]) -> ok.

%% @doc Start the application's root supervisor in erl listener.
start() ->
    mq_speed_sup:start_link().

%% @doc Start the application's root supervisor from boot.
start(_StartType, _StartArgs) ->
    mq_speed_sup:start_link().

%% @doc Stop the application.
stop(_State) -> ok.
