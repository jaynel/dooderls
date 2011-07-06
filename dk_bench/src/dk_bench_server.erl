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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE). 

-record(dkb_state, {}).

%% External function specifications


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


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

-type call_rqst() :: {mq_raw, pos_integer} |{mq_raw, pos_integer(), pos_integer()} | any().
-spec handle_call(call_rqst(), {pid(), reference()}, #dkb_state{})
                 -> {reply, {mq_raw, list()} | ok, #dkb_state{}}.

%% Interface for requesting benchmark runs.
handle_call({mq_raw, NumMsgs}, _From, #dkb_state{} = State) ->
    [{proc_lib, NumMsgs, Props}] = mq_raw:run_test(NumMsgs, 1, mq_data:msgs()),
     TimingProps = [ {Key, {Micros / 1000, milliseconds},
                      {Micros / NumMsgs, microseconds_per_msg}}
                     || {Key, Micros} <- Props ],
    {reply, {mq_raw, TimingProps}, State};
handle_call({mq_raw, NumMsgs, TimesToRun}, _From, #dkb_state{} = State) ->
    PropList = mq_raw:run_test(NumMsgs, TimesToRun, mq_data:msgs()),
    TimingProps =
        [
         [
          {Key, {Micros / 1000, ms},
           {int_ceil(Micros / NumMsgs * 1000),
            nanos_per_msg}} || {Key, Micros} <- Props
         ] || {proc_lib, _N, Props} <- PropList
        ],
    {reply, {mq_raw, TimingProps}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(any(), #dkb_state{}) -> {noreply, #dkb_state{}}.
-spec handle_info(any(), #dkb_state{}) -> {noreply, #dkb_state{}}.

handle_cast(_Msg, State) ->  {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

int_ceil(X) ->
     T = trunc(X),
     if
         X > T -> T + 1;
         true  -> T
     end.

