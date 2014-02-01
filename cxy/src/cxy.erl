-module(cxy).
-author('jay@duomark.com').

%% Redis accessor calls
-export([start/0, stop/0, start/2, stop/1]).

%% Shell starting...
-spec start() -> ok | {error, {already_started, ?MODULE}}.
start() -> application:start(cxy).

-spec stop() -> ok.
stop() -> application:stop(cxy).

%% Release starting...
-spec start(any(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    cxy_sup:start_link().

%% @private
-spec stop(any()) -> ok.
stop(_State) -> ok.
