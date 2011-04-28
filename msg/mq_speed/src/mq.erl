-module(mq).

-export([start_link/0, loop/0]).

start_link() ->
    {ok, proc_lib:spawn(?MODULE, loop, [])}.

loop() ->
    receive
        _Any -> ok
    end.

            
