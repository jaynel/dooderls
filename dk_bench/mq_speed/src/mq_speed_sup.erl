-module(mq_speed_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, StartFn, Type), {I, {I, StartFn, []}, permanent, 5000, Type, [I]}).

-spec start_link() -> {ok, pid()}.
-spec init(Args::{}) -> {ok, any()}.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({}) ->
    %% RawServer = ?CHILD(mq, start_loop, worker),
    %% GenServer = ?CHILD(mq, start_gen, worker),
    {ok, { {one_for_one, 5, 10},
           [
            %% RawServer
            %% , GenServer
           ]} }.

