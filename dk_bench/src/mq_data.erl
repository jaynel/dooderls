%% ============================================================================
%% Msg data used in testing message queue speed
%% ============================================================================
-module(mq_data).

-export([msgs/0]).


-type msg_set() :: {binary(), binary(), binary(), binary(), binary(),
                    binary(), binary(), binary(), binary(), binary()}.
-export_type([msg_set/0]).

-spec msgs() -> msg_set().


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

