-module(inc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.

start_link() ->
    error_logger:info_msg("Starting ~p:start_link/0~n", [?MODULE]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-type restart() :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()}.
-spec init({}) -> {ok, {restart(), [supervisor:child_spec()]}}.

 %% Helper macros for declaring children of supervisor
-define(SUPER(I, ARGS), {I, {I, start_link, ARGS}, permanent, infinity, supervisor, [I]}).
-define(CHILD(I, ARGS), {I, {I, start_link, ARGS}, transient, 5000, worker, [I]}).

init({}) ->
    IncLagerSup = ?SUPER(inc_lager_sup, []),
    IncServer = ?CHILD(inc_server, []),
    {ok, { {one_for_one, 5, 10}, [IncLagerSup, IncServer]} }.

