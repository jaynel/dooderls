%% ============================================================================
%% Testing the speed of function calls
%% ============================================================================
-module(fun_calls).

-license("New BSD").
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% External access API...
-export([run_exec_test/3, run_access_test/4, loop/3, loop/4]).

%% Choice of tests to run.
-export([function_call/1, mfa_call/3]).
-export([list_nth/2, list_head/2, binary_raw/2, binary_at/2, tuple_inx/2]).

-include("dk_bench.hrl").

-type timing_result()   :: {loop_time, pos_integer()}.
-type run_result()      :: {proc_lib, pos_integer(), [timing_result(), ...]}.

-spec wait_and_get_results(pos_integer()) ->  run_result() | none.
-spec run_exec_test(valid_exec_types(),
                    pos_integer(), pos_integer())
                   -> [run_result() | none].
-spec run_access_test(valid_access_types(),
                      pos_integer(), pos_integer(), pos_integer())
                     -> [run_result() | none].

%% Loop running a benchmark that executes a number of times.
run_exec_test(Fun, NumExecs, TestTimes) ->
    run_exec_test(Fun, NumExecs, TestTimes, []).

run_exec_test(_Fun, _NumExecs, 0, Results) -> Results;
run_exec_test(Fun, NumExecs, TestTimes, Results)
  when is_integer(NumExecs), is_integer(TestTimes) ->
    garbage_collect(),
    {ok, _P1} = start_loop(Fun, self(), NumExecs),
    NewResults = wait_and_get_results(NumExecs),
    run_exec_test(Fun, NumExecs, TestTimes-1, [NewResults | Results]).

%% Loop running a benchmark that generates a random length structure
%% and then accesses it randomly a given number of times.
run_access_test(Fun, DataSize, NumAccesses, TestTimes) ->
    run_access_test(Fun, DataSize, NumAccesses, TestTimes, []).

run_access_test(_Fun, _DataSize, _NumAccesses, 0, Results) -> Results;
run_access_test(Fun, DataSize, NumAccesses, TestTimes, Results)
  when is_integer(DataSize), is_integer(NumAccesses), is_integer(TestTimes) ->
    garbage_collect(),
    {ok, _P1} = start_loop(Fun, self(), DataSize, NumAccesses),
    NewResults = wait_and_get_results(NumAccesses),
    run_access_test(Fun, DataSize, NumAccesses, TestTimes-1, [NewResults | Results]).

wait_and_get_results(Count) ->
    receive
        {proc_lib, Count, LoopMicros} ->
            {proc_lib, Count, [{loop_time, LoopMicros}]}
    after
        4000 -> none
    end.


%% ============= proc_lib =====================================================
-spec loop(valid_exec_types(), pid(), pos_integer()) -> ok.
-spec start_loop(valid_exec_types(), pid(), pos_integer()) -> {ok, pid()}.
-spec loop(valid_access_types(), pid(), pos_integer(), pos_integer()) -> ok.
-spec start_loop(valid_access_types(), pid(), pos_integer(), pos_integer()) -> {ok, pid()}.

start_loop(Fun, Caller, LoopCount) ->
    Pid = proc_lib:spawn(?MODULE, loop, [Fun, Caller, LoopCount]),
    {ok, Pid}.

loop(Fun, Caller, LoopCount) ->
    Args = case Fun of
               function_call ->  [LoopCount];
               mfa_call ->       [?MODULE, mfa_call, LoopCount]
           end,
    {Time, _Val} = timer:tc(?MODULE, Fun, Args),
    Caller ! {proc_lib, LoopCount, Time},
    ok.

start_loop(Fun, Caller, DataSize, LoopCount) ->
    Pid = proc_lib:spawn(?MODULE, loop, [Fun, Caller, DataSize, LoopCount]),
    {ok, Pid}.

loop(Fun, Caller, DataSize, LoopCount) ->
    Args = case Fun of
               tuple_inx -> make_tuple_args(DataSize, LoopCount);
               list_head -> make_list_args(DataSize, LoopCount);
               list_h_t ->  make_list_args(DataSize, LoopCount);
               list_nth ->  make_list_args(DataSize, LoopCount);
               binary_at -> make_bin_args(DataSize, LoopCount);
               binary_raw ->
                   [Inxs, Bin] = make_bin_args(DataSize, LoopCount),
                   [[X*8 || X <- Inxs], Bin]
           end,
    {Time, _Val} = timer:tc(?MODULE, Fun, Args),
    Caller ! {proc_lib, LoopCount, Time},
    ok.


%% ============= util funs =====================================================
make_list_args(ListSize, LoopCount) ->
    List = lists:seq(1, ListSize),
    RepeatTimes = case ListSize > LoopCount of
                      true ->  LoopCount;
                      false -> ListSize - 1
                  end,
    [RepeatTimes, List].

make_bin_args(BinSize, LoopCount) ->    
    Bin = make_bin(BinSize),
    Inxs = make_random_inxs(LoopCount, BinSize),
    [Inxs, Bin ].

make_tuple_args(TupleSize, LoopCount) ->
    Tuple = make_tuple(TupleSize),
    Inxs = make_random_inxs(LoopCount, TupleSize),
    [Inxs, Tuple].

make_bin(Size) ->   list_to_binary(lists:seq(1,Size)).
make_tuple(Size) -> list_to_tuple(lists:seq(1,Size)).

%% Random indexes are in the range 1 - N-1 so that bin and tuple work.
make_random_inxs(Num, MaxInx) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    make_random_inxs(Num, MaxInx-1, []).

make_random_inxs(0, _MaxInx, Inxs) -> Inxs;
make_random_inxs(N, MaxInx, Inxs) when N > 0 -> 
    make_random_inxs(N-1, MaxInx, [random:uniform(MaxInx) | Inxs]).


%% ============= exec funs =====================================================
-spec function_call(non_neg_integer()) -> ok.
-spec mfa_call(module(), atom(), non_neg_integer()) -> ok.

function_call(0) -> ok;
function_call(Count) when Count > 0 ->
    function_call(Count-1).

mfa_call(_M, _F, 0) -> ok;
mfa_call(M, F, Count) when Count > 0 ->
    M:F(M, F, Count-1).


%% ============= access funs =====================================================
-spec list_nth(non_neg_integer(), list(pos_integer())) -> ok.
-spec list_head(non_neg_integer(), list(pos_integer())) -> ok.
-spec binary_at([non_neg_integer(),...],  binary()) -> [byte()].
-spec binary_raw([non_neg_integer(),...], binary()) -> [byte()].
-spec tuple_inx([pos_integer(),...],      tuple(integer())) -> [pos_integer()].

list_nth(N, L) -> lists:nth(N,L).
    
list_head(0, _List) -> ok;
list_head(1, [_H]) -> ok;
list_head(N, [_H|T]) when N > 0 -> 
    list_head(N-1, T).
    
binary_raw(Positions, Bin) ->
    [ begin << _B:P, C, _Rest/binary >> = Bin, C end || P <- Positions ].
                
binary_at(Positions, Bin) ->
    [ binary:at(Bin, P) || P <- Positions ].
                
tuple_inx(Positions, Tuple) ->
    [ element(P, Tuple) || P <- Positions ].
