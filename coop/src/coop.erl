%%%------------------------------------------------------------------------------
%%% @copyright (c) 2012, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Benchmark server for invoking experimental performance tests.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(coop).

-license("New BSD").
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% API
-export([new/2, new/1]).


-type built_in_topology() :: pipeline | round_robin | custom.

-record(coop_state, {
          curr_pos = 1 : non_neg_integer(),
          minions = {} : tuple(),
          type = built_in_topology()
         }).

-spec new(built_in_topology(), pos_integer()) -> #coop_state() | {error, any()}.

new(pipeline,    N) when is_integer(N), N > 0 -> launch(pipeline);
new(round_robin, N) when is_integer(N), N > 0 -> launch(round_robin).

launch(CoopType) ->
    Type = list_to_tuple([proc_lib:spawn_link() || _X <- lists:seq(1,N)]),
    TSz = tuple_size(T),
    case T of
        N -> #coop_pipline_state{minions=T, type=CoopType};
        _X -> {error, too_many_procs}
    end.
            

%% A topology is a diagraph with the following form:
%%    Vertex: Becomes a single pid
%%    Edge:   A fuction f(term()) -> term()
%%    Label:  Process name to register or {}
-spec new(digraph:digraph()) -> ok | {error, any()}.

new(Topology) -> ok.
    

%% Ingest provides new data to the coop.
ingest(#coop_state{type=round_robin_curr_pos=CurrPos, minions = M} = State, Value) ->
    MSize = tuple_size(M),
    case CurrPos > MSize of
        true  -> element(1, M) ! Value, State#coop_state{curr_pos=2};
        false -> M ! Value, State#coop_state{curr_pos=CurrPos + 1}
    end;
ingest(#coop_state{type=pipeline, minions = M} = State, Value) ->
    element(1, M) ! Value, State.

%% Emit is used to produce data from a process.
emit(#coop_state{type=pipeline, minions = M} = State) -> pull(element(tuple_size(M), M));
emit(#coop_state{type=round_robin, minions = M} = State) -> pull(element(tuple_size(M), M)).
