-module(inc_server).

-compile([{parse_transform, lager_transform}]).


-export([start_link/0, run/0, get_app_env/2]).

start_link() ->
    error_logger:info_msg("Starting ~p:start_link/0~n", [?MODULE]),
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.

run() ->
    error_logger:info_msg("Starting ~p:run/0~n", [?MODULE]),
    lager:log(info, self(), "inc_server start"),
    lager:info("inc_server started 2"),
    receive stop -> ok
    after 100000 -> ok
    end,
    error_logger:info_msg("Stopping ~p:run/0~n", [?MODULE]),
    lager:info("inc_server ended").


%%--------------------------------------------------------------------
%% @doc
%%   Get config parameter for the running application.
%%
%%   Check the current application context, then the init
%%   context, and finally return a default if neither has
%%   a value.
%% @end
%%--------------------------------------------------------------------
-spec get_app_env(atom(), any()) -> any().

get_app_env(Param, Default) ->
    case application:get_env(Param) of
        {ok, Val} -> Val;
        undefined ->
            case init:get_argument(Param) of
                {ok, [[FirstVal | _OtherVals], _MoreVals]} -> FirstVal;
                error -> Default
            end
    end.
             
