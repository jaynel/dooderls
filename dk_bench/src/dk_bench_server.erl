%%%-------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Benchmark server for invoking experimental performance tests.
%%% @since v0.0.1
%%% @end
%%%-------------------------------------------------------------------
-module(dk_bench_server).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         mq_raw/1, mq_raw/2,
         exec/2, exec/3,
         access/3, access/4
        ]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("dk_bench.hrl").

-define(SERVER, ?MODULE). 

-record(dkb_state, {}).

%% External function specifications


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.

%% @doc Start the benchmark server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


%% Interface for testing message queue speed.
-spec mq_raw(pos_integer()) -> list().
-spec mq_raw(pos_integer(), pos_integer()) -> list().

%% @doc Test message queuing speed with NumMsgs.
mq_raw(NumMsgs) ->
    gen_server:call(?SERVER, {mq_raw, NumMsgs}).

%% @doc Call a simple function repeating it TimesToRun times.
mq_raw(NumMsgs, TimesToRun) ->
    gen_server:call(?SERVER, {mq_raw, NumMsgs, TimesToRun}).


%% Interface for testing simple execution speeds.
-spec exec(valid_exec_types(), pos_integer()) -> list().
-spec exec(valid_exec_types(), pos_integer(), pos_integer()) -> list().

%% @doc Call a simple function LoopCount times.
exec(Fun, LoopCount) ->
    gen_server:call(?SERVER, {exec, Fun, LoopCount}).

%% @doc Call a simple function repeating it TimesToRun times.
exec(Fun, LoopCount, TimesToRun) ->
    gen_server:call(?SERVER, {exec, Fun, LoopCount, TimesToRun}).


%% Interface for testing data access speeds.
-spec access(valid_access_types(), pos_integer(), pos_integer()) -> list().
-spec access(valid_access_types(), pos_integer(), pos_integer(), pos_integer()) -> list().

%% @doc Call a simple function LoopCount times.
access(Fun, DataSize, LoopCount) ->
    gen_server:call(?SERVER, {access, Fun, DataSize, LoopCount}).

%% @doc Call a simple function repeating it TimesToRun times.
access(Fun, DataSize, LoopCount, TimesToRun) ->
    gen_server:call(?SERVER, {access, Fun, DataSize, LoopCount, TimesToRun}).
    

%%%===================================================================
%%% init, terminate, code_change callbacks
%%%===================================================================

-spec init({}) -> {ok, #dkb_state{}}.
-spec terminate(atom(), #dkb_state{}) -> ok.
-spec code_change(string(), #dkb_state{}, any()) -> {ok, #dkb_state{}}.

init({}) -> {ok, #dkb_state{}}.

%% Unused gen_stream exported callbacks.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%===================================================================
%%% handle message callbacks
%%%===================================================================

-type mq_call_rqst() ::  {mq_raw, pos_integer()}
                       | {mq_raw, pos_integer(), pos_integer()}.

-type exec_call_rqst() :: {exec, valid_exec_types(), pos_integer()}
                       | {exec, valid_exec_types(), pos_integer(), pos_integer()}.

-type access_call_rqst() :: {access, valid_access_types(), pos_integer(), pos_integer()}
                          | {access, valid_access_types(), pos_integer(), pos_integer()}.

-spec handle_call(mq_call_rqst() | exec_call_rqst() | access_call_rqst(),
                  {pid(), reference()}, #dkb_state{})
                 -> {reply, {mq_raw, list()} | ok, #dkb_state{}}.

%% Interface for requesting benchmark runs.
handle_call({mq_raw, NumMsgs}, _From, #dkb_state{} = State) ->
    {reply, {mq_raw, get_mq_results(NumMsgs)}, State};
handle_call({mq_raw, NumMsgs, TimesToRun}, _From, #dkb_state{} = State) ->
    {reply, {mq_raw, get_mq_results(NumMsgs, TimesToRun)}, State};
handle_call({exec, Fun, LoopCount}, _From, #dkb_state{} = State) ->
    {reply, {exec, get_exec_results(Fun, LoopCount)}, State};
handle_call({exec, Fun, LoopCount, TimesToRun}, _From, #dkb_state{} = State) ->
    {reply, {exec, get_exec_results(Fun, LoopCount, TimesToRun)}, State};
handle_call({access, Fun, DataSize, LoopCount}, _From, #dkb_state{} = State) ->
    {reply, {access, get_access_results(Fun, DataSize, LoopCount)}, State};
handle_call({access, Fun, DataSize, LoopCount, TimesToRun}, _From, #dkb_state{} = State) ->
    {reply, {access, get_access_results(Fun, DataSize, LoopCount, TimesToRun)}, State}.


-spec handle_cast(any(), #dkb_state{}) -> {noreply, #dkb_state{}}.
-spec handle_info(any(), #dkb_state{}) -> {noreply, #dkb_state{}}.

handle_cast(_Msg, State) ->  {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_exec_results(Fun, LoopCount) ->
    rpt_results(fun_calls:run_exec_test(Fun, LoopCount, 1)).
get_exec_results(Fun, LoopCount, TimesToRun) ->
    rpt_results(fun_calls:run_exec_test(Fun, LoopCount, TimesToRun)).

get_access_results(Fun, DataSize, LoopCount) ->
    rpt_results(fun_calls:run_access_test(Fun, DataSize, LoopCount, 1)).
get_access_results(Fun, DataSize, LoopCount, TimesToRun) ->
    rpt_results(fun_calls:run_access_test(Fun, DataSize, LoopCount, TimesToRun)).

get_mq_results(NumMsgs) ->
    rpt_results(mq_raw:run_test(NumMsgs, 1, mq_data:msgs())).
get_mq_results(NumMsgs, TimesToRun) ->
    rpt_results(mq_raw:run_test(NumMsgs, TimesToRun, mq_data:msgs())).

rpt_results([{proc_lib, NumMsgs, Props}]) ->
    [ {Key, {Micros / 1000, milliseconds}, {Micros / NumMsgs, microseconds_per_msg}}
      || {Key, Micros} <- Props ];
rpt_results(PropList) ->
    [
     [
      {Key, {Micros / 1000, ms}, {int_ceil(Micros / NumMsgs * 1000), nanos_per}}
      || {Key, Micros} <- Props
     ]  || {proc_lib, NumMsgs, Props} <- PropList
    ].

int_ceil(X) ->
     T = trunc(X),
     if
         X > T -> T + 1;
         true  -> T
     end.

