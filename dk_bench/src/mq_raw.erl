%% ============================================================================
%% Unloading a full message queue in a proc_lib process.
%% ============================================================================
-module(mq_raw).

-license("New BSD").
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-export([run_test/3]).
-export([send_msgs/2, loop/1]).
-export([unload_all_proc_lib_msgs/1, unload_any_proc_lib_msgs/1]).

-type timing_result() :: {send_time | recv_time, pos_integer()}.
-type run_result() :: {proc_lib, pos_integer(), [timing_result(), ...]}.

-spec run_test(pos_integer(), pos_integer(), mq_data:msg_set()) -> run_result() | none.
-spec wait_and_get_results(pos_integer(), pos_integer()) ->  run_result() | none.
-spec gen_msgs(non_neg_integer(), mq_data:msg_set(), list()) -> list().
-spec send_msgs(pid(), list()) -> {last}.

-define(QUEUE_FILL_TIME, 60000).


%% Start a new process to hold messages, fill up its mailbox queue,
%% then dequeue all the messages and report timing.
run_test(MsgCount, NumTimes, Msgs) ->
    run_test(MsgCount, NumTimes, Msgs, []).

run_test(_MsgCount, 0, _Msgs, Results) ->
    Results;
run_test(MsgCount, NumTimes, Msgs, Results)
  when is_integer(MsgCount), is_integer(NumTimes), is_tuple(Msgs) ->
    {ok, P1} = start_loop(self()),
    MsgList = gen_msgs(MsgCount, Msgs, []),
    garbage_collect(),
    {SendTime, {last}} = timer:tc(?MODULE, send_msgs, [P1, MsgList]),
    NewResults = wait_and_get_results(MsgCount, SendTime),
    run_test(MsgCount, NumTimes-1, Msgs, [NewResults | Results]).

wait_and_get_results(Count, SendMicros) ->
    receive
        {proc_lib, Count, RecvMicros} ->
            {proc_lib, Count, [{send_time, SendMicros}, {recv_time, RecvMicros}]}
    after
        ?QUEUE_FILL_TIME -> none
    end.

%% Generate N msgs randomly selected from a pre-built tuple.
gen_msgs(0, _SrcMsgs, GenMsgs) ->
    GenMsgs;
gen_msgs(N, SrcMsgs, GenMsgs) ->
    M = random:uniform(size(SrcMsgs)),
    gen_msgs(N-1, SrcMsgs, [element(M, SrcMsgs) | GenMsgs]).

    
%% Send msgs as quickly as possible to another process.
%% The msg {last} is sent to signal he queue is full.
send_msgs(Pid, []) ->
    Pid ! {last};
send_msgs(Pid, [Msg|Msgs]) ->
    Pid ! {msg, self(), Msg},
    send_msgs(Pid, Msgs).


%% ============= proc_lib =====================================================
-spec loop(pid()) -> ok.
-spec start_loop(pid()) -> {ok, pid()}.
-spec unload_all_proc_lib_msgs(pos_integer()) -> pos_integer().
-spec unload_any_proc_lib_msgs(pos_integer()) -> pos_integer().

start_loop(Caller) ->
    Pid = proc_lib:spawn(?MODULE, loop, [Caller]),
    %% register(mq_spawn, Pid),
    {ok, Pid}.

loop(Caller) ->
    receive {last} -> ok
    after   ?QUEUE_FILL_TIME -> ok
    end,
    %% garbage_collect(),
    %% {Time, Count} = timer:tc(?MODULE, unload_all_proc_lib_msgs, [0]),
    {Time, Count} = timer:tc(?MODULE, unload_any_proc_lib_msgs, [0]),
    Caller ! {proc_lib, Count, Time},
    ok.

unload_all_proc_lib_msgs(Count) ->
    receive
        {msg, _Pid, Msg} when is_binary(Msg) ->
            unload_all_proc_lib_msgs(Count + 1)
    after
        0 -> Count
    end.

unload_any_proc_lib_msgs(Count) ->
    receive
        _Any -> unload_any_proc_lib_msgs(Count + 1)
    after
        0 -> Count
    end.

