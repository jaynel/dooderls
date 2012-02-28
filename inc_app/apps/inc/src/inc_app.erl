-module(inc_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, start_phase/3, stop/1]).

-include("inc_lager.hrl").


%% ===================================================================
%% Application callbacks
%% ===================================================================

-record(app_state, {saved_handlers}).

-spec start() -> {ok, pid(), #app_state{}}.
-spec start(any(), any()) -> {ok, pid(), #app_state{}}.
-spec stop(#app_state{}) -> ok.

%% @doc Start the application's root supervisor in erl listener.
start() ->
    error_logger:info_msg("Starting ~p:start/0~n", [?MODULE]),
    {ok, Pid} = inc_sup:start_link(),
    finish_start(Pid).

finish_start(Pid) ->
    Handlers = inc_lager_sup:setup_lager_handlers(),
    {ok, Pid, #app_state{saved_handlers = Handlers}}.

%% @doc Start the application's root supervisor from boot.
start(StartType, StartArgs) ->
    ?TT_INFO_MSG("LAGER: Starting", []),
    error_logger:info_msg("Starting ~p:start(~p, ~p)~n",
                          [?MODULE, StartType, StartArgs]),
    {ok, Pid} = inc_sup:start_link(),
    error_logger:info_msg("Ending ~p:start(~p, ~p)~n",
                          [?MODULE, StartType, StartArgs]),
    finish_start(Pid).

%% @doc Progress through orderly start up using separate phases.
start_phase(init, normal, []) ->
    error_logger:info_msg("Starting ~p:start_phase(init, normal, [])~n", [?MODULE]),
    ok;
start_phase(go, normal, []) ->
    error_logger:info_msg("Starting ~p:start_phase(go, normal, [])~n", [?MODULE]),
    ok.

%% @doc Stop the application.
stop(#app_state{saved_handlers=Handlers}) ->
    error_logger:info_msg("Stopping ~p:stop/1~n", [?MODULE]),
    [error_logger:add_report_handler(Handler) || Handler <- Handlers],
    error_logger:info_msg("Stopped application ~p~n", [?MODULE]),
    ok.


