-module(inc_lager_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, setup_lager_handlers/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("lager/include/lager.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-define(SERVER, ?MODULE).

-type error_handler() :: atom().

-spec start_link() -> {ok, pid()}.
-spec setup_lager_handlers() -> [error_handler()].

start_link() ->
    error_logger:info_msg("Starting ~p:start_link/0~n", [?MODULE]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

setup_lager_handlers() ->
    Handlers = case application:get_env(lager, handlers) of
        {ok, Val} -> Val;
        undefined ->
            [{lager_console_backend, info},
                {lager_file_backend, [{"log/error.log", error, 10485760, "", 5},
                        {"log/console.log", info, 10485760, "", 5}]}]
    end,
    [supervisor:start_child(lager_handler_watcher_sup, [lager_event, Module, Config]) || {Module, Config} <- Handlers],

    %% mask the messages we have no use for
    MinLog = lager:minimum_loglevel(lager:get_loglevels()),
    lager_mochiglobal:put(loglevel, MinLog),

    case application:get_env(lager, error_logger_redirect) of
        {ok, false} -> [];
        _ ->
            supervisor:start_child(lager_handler_watcher_sup, [error_logger, error_logger_lager_h, []]),
            %% Should we allow user to whitelist handlers to not be removed?
            [begin error_logger:delete_report_handler(X), X end ||
                X <- gen_event:which_handlers(error_logger) -- [error_logger_lager_h]]
    end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-type restart() :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()}.
-spec init({}) -> {ok, {restart(), [supervisor:child_spec()]}}.

 %% Helper macros for declaring children of supervisor
-define(SUPER(I, ARGS), {I, {I, start_link, ARGS}, permanent, infinity, supervisor, [I]}).

init({}) ->
    %% until lager is completely started, allow all messages to go through
    lager_mochiglobal:put(loglevel, ?DEBUG),
    LagerSup = ?SUPER(lager_sup, []),
    {ok, { {one_for_one, 5, 10}, [LagerSup]} }.
