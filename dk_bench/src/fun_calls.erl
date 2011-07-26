%% ============================================================================
%% Testing the speed of function calls
%% ============================================================================
-module(fun_calls).

-license("New BSD").
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% External access API...
-export([run_test/3, loop/3]).

%% Choice of tests to run.
-export([fun_call/1, bin_comp/2, tuple_inx/2]).

-include("dk_bench.hrl").

-type timing_result()   :: {loop_time, pos_integer()}.
-type run_result()      :: {proc_lib, pos_integer(), [timing_result(), ...]}.

-spec wait_and_get_results(pos_integer()) ->  run_result() | none.
-spec run_test(valid_fun_types(), pos_integer(), pos_integer())
              -> [run_result() | none].


%% Loop calling a function that only increments its argument.
run_test(Fun, LoopCount, NumTimes) ->
    run_test(Fun, LoopCount, NumTimes, []).

run_test(_Fun, _LoopCount, 0, Results) ->
    Results;
run_test(Fun, LoopCount, NumTimes, Results)
  when is_integer(LoopCount), is_integer(NumTimes) ->
    garbage_collect(),
    {ok, _P1} = start_loop(self(), LoopCount, Fun),
    NewResults = wait_and_get_results(LoopCount),
    run_test(Fun, LoopCount, NumTimes-1, [NewResults | Results]).

wait_and_get_results(Count) ->
    receive
        {proc_lib, Count, LoopMicros} ->
            {proc_lib, Count, [{loop_time, LoopMicros}]}
    after
        10000 -> none
    end.


%% ============= proc_lib =====================================================
-spec loop(pid(), pos_integer(), valid_fun_types()) -> ok.
-spec start_loop(pid(), pos_integer(), valid_fun_types()) -> {ok, pid()}.

start_loop(Caller, LoopCount, Fun) ->
    Pid = proc_lib:spawn(?MODULE, loop, [Caller, LoopCount, Fun]),
    {ok, Pid}.

loop(Caller, LoopCount, Fun) ->
    Args = case Fun of
               fun_call ->  [LoopCount];
               bin_comp ->  [[3,5,7,9], << 0:(8*LoopCount) >> ];
               tuple_inx -> [[3,5,7,9], erlang:make_tuple(LoopCount, 27)]
           end,
    {Time, _Val} = timer:tc(?MODULE, Fun, Args),
    Caller ! {proc_lib, LoopCount, Time},
    ok.


%% ============= test funs =====================================================
-spec fun_call(non_neg_integer()) -> ok.
-spec bin_comp([non_neg_integer()], binary()) -> [byte()].
-spec tuple_inx([pos_integer()], tuple(integer())) -> [integer()].

fun_call(0) -> ok;
fun_call(Count) ->
    fun_call(Count-1).

bin_comp(Positions, Bin) ->
    [ begin
          << _B:P, C, _Rest/binary >> = Bin,
          C
      end || P <- Positions].
                
tuple_inx(Positions, Tuple) ->
    [ element(P, Tuple) || P <- Positions ].
