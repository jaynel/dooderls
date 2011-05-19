%% ============================================================================
%% Unloading a full message queue in a gen_server vs. a proc_lib process.
%%   mq:run_test() reports the results of multiple message queue lengths.
%% ============================================================================
-module(mq).

-license("New BSD").
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% Function to control test
-export([run_test/0, run_test/1]).
-export([loop/1, unload_all_proc_lib_msgs/2]).
-export([start_gen/0, queue_gen_msg/2]).

%% External API for gen_stream
-export([init/1, handle_info/2, terminate/2]).


-record(gen_state, {count=0}). %, start_time, end_time}).
% -record(proc_state, {count=0, start_time, end_time}).

%% 100K messages in queue.
-define(LAST_MSG, 99999).
-define(QUEUE_FILL_TIME, 20000).

run_test() ->
    [begin 
         {proc_lib, C, Micros} = run_test(C),
         {proc_lib, C, {Micros / 1000, milliseconds}, {Micros / C, microseconds_per_msg}}
     end || C <- [1000, 10000, 100000, 1000000]].

run_test(Count) ->
    Msgs = msgs(),
    garbage_collect(),
    {ok, P1} = start_loop(self()),
    send_msgs(P1, Count, Msgs),
    get_results().

get_results() ->
    timer:sleep(?QUEUE_FILL_TIME),
    receive
        Any -> Any
    after
        1000 -> none
    end.

send_msgs(_Pid, 0, _Msgs) -> ok;
send_msgs(Pid, Count, Msgs) ->
    M = random:uniform(size(Msgs)),
    Pid ! {msg, self(), element(M, Msgs)},
    send_msgs(Pid, Count-1, Msgs).

msgs() ->
    {
      <<"A simple message of more than 64 bytes to make sure it is in the binary heap">>,
      <<"Second message which is used as an alternative binary to send to the processes">>,
      <<"Whatever we need to use to fill up the message queue with roughly similar sized msgs">>,
      <<"More data that can be pumped into the message queues before measuring unload speed">>,
      <<"Another collection of strings to put into the binary heap to fill message queues">>,
      <<"The collection of messages won't cause a memory overflow, but a little variance">>,
      <<"Putting lots into a message queue will cause sharing of binary pointers not duplication">>,
      <<"Using a tuple is a common method of tagging messages in most applications">>,
      <<"This test is very artificial but will give a sense of any dramatic performance difference">>,
      <<"It's not a memory test, but a comparison of mailbox access and message retrieval speed">>
    }.


%% ============= gen_server ===================================================
start_gen() ->
    gen_server:start_link({local, mq_gen}, ?MODULE, {}, [{timeout, 15000}]).

init({}) ->
    {ok, #gen_state{}}.

queue_gen_msg(Server, Msg) ->
    gen_server:cast(Server, Msg).

%% Pause to let queue fill when first message unloaded...
handle_info(_Msg, #gen_state{count=0} = State) ->
    timer:sleep(?QUEUE_FILL_TIME),
    {noreply, State#gen_state{count=1}};

%% Set end time when last msg is retrieved...
handle_info(_Msg, #gen_state{count=?LAST_MSG} = State) ->
    {noreply, State#gen_state{count=?LAST_MSG+1}};

%% Just count the rest.
handle_info(_Msg, #gen_state{count=Count} = State) ->
    {noreply, State#gen_state{count=Count+1}}.

terminate(_Reason, _State) -> ok.


%% ============= proc_lib =====================================================
start_loop(Caller) ->
    Pid = proc_lib:spawn(?MODULE, loop, [Caller]),
    %% register(mq_spawn, Pid),
    {ok, Pid}.

loop(Caller) ->
    garbage_collect(),
    timer:sleep(?QUEUE_FILL_TIME),
    {Time, {_Msgs, Count}} = timer:tc(?MODULE, unload_all_proc_lib_msgs, [[], 0]),
    Caller ! {proc_lib, Count, Time}.

unload_all_proc_lib_msgs(Msgs, Count) ->
    receive
        {msg, _Pid, Msg} when is_binary(Msg) ->
            unload_all_proc_lib_msgs([Msg | Msgs], Count + 1)
    after
        0 -> {Msgs, Count}
    end.
