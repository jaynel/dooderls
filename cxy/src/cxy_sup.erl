-module(cxy_sup).
-author('jay@duomark.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%% ===================================================================
%% API functions
%% ===================================================================

%% From OTP/lib/stdlib/src/supervisor.erl
-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-spec start_link() -> startlink_ret().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-type restart() :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()}.
-type sup_init_return() :: {ok, {restart(), [supervisor:child_spec()]}}.

-spec init({}) -> sup_init_return().


%% Helper macro for declaring children of supervisor
-define(CHILD(__Mod, __Type), {__Mod, {__Mod, start_link, []}, __Type, 5000, worker, [__Mod]}).
-define(CHILD(__Name, __Mod, __Args), {__Name, {__Mod, start_link, __Args}, permanent, 5000, worker, [__Mod]}).
-define(SUPER(__Mod, __Args), {__Mod, {__Mod, start_link, __Args}, permanent, infinity, supervisor, [__Mod]}).
-define(SUPER(__Name, __Mod, __Args), {__Name, {__Mod, start_link, __Args}, permanent, infinity, supervisor, [__Name]}).

init({}) ->
    error_logger:info_msg(" Initializing ~p supervisor~n", [?SERVER]),

    %% Top supervisors...
    Cxy_Queue  = ?CHILD(cxy_queue, permanent),
    Children = [Cxy_Queue],

    {ok, { {rest_for_one, 5, 60}, Children} }.
