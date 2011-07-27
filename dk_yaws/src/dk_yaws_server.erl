-module(dk_yaws_server).

-export([start_link/0, run/0]).

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.

run() ->
    Id = "yaws_embedded",
    GconfList = [{id, Id}],
    Docroot = get_app_env(docroot, "/var/yaws/www"),
    SconfList = [{port, 8888},
                 {servername, "yon"},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(dk_yaws_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.


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
             
