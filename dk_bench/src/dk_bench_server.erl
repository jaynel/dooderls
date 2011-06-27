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
-spec start_link() -> {ok, pid()}.
-spec init({}) -> {ok, #dkb_state{}}.
-spec terminate(atom(), #dkb_state{}) -> ok.
-spec code_change(string(), #dkb_state{}, any()) -> {ok, #dkb_state{}}.

-spec handle_cast(any(), #dkb_state{}) -> {noreply, #dkb_state{}}.
-spec handle_info(any(), #dkb_state{}) -> {noreply, #dkb_state{}}.
-spec handle_call(atom(), {pid(), reference()}, #dkb_state{})
                 -> {reply, ok, #dkb_state{}}.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({}) -> {ok, #dkb_state{}}.

%% Unused gen_stream exported callbacks.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Msg, State) ->  {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

%% Interface for requesting benchmark runs.
handle_call(_Request, _From, State) -> {reply, ok, State}.
