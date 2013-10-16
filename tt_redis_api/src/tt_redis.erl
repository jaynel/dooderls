%%%------------------------------------------------------------------------------
%%% @copyright (c) 2013, TigerText, Inc.
%%% @author Jay Nelson <jay@tigertext.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Interface to redis calls via redis_cxn gen_server.
%%% @end
%%%------------------------------------------------------------------------------
-module(tt_redis).
-author('jay@tigertext.com').

%% Redis accessor calls
-export([connect/2, quit/1, call/2, pipe/2, exec/2]).

-export([
         brpoplpush/5, del/2, mdel/2, eval/2, eval/4, exists/2, expire/3, get/2, getbit/3,
         hdel/3, hget/3, hgetall/2, hincrby/4, hkeys/2, hmget/3,
         hmset/3, hset/4, hsetnx/4, incr/2, incrby/3, keys/2,
         llen/2, lpush/3, ltrim/4, lrange/4, lrem/4,
         mget/2, publish/3, rpush/3,
         sdiffstore/3, sunionstore/3,
         srandmember/3, sadd/3, sadd_multi/3, saddcount/3, scard/2, sdiff/2,
         set/3, setbit/4, setnx/3, sismember/3, sunion/2,
         srandmember/2, smembers/2, sort/3, srem/3, smove/4, type/2,
         zadd/4, zrem/3, zrangebyscore/5, zrangebyscore/4
        ]).


%% ------------------------------------
%%  External interface
%% ------------------------------------

-type key()    :: string() | binary() | iolist().
-type value()  :: integer() | atom() | list() | binary().
-type server() :: atom().
-type string_value() :: binary() | nil.

-spec connect(atom(), pos_integer()) -> {ok, pid()}.
-spec quit(pid()) -> ok.

-spec brpoplpush(pid(), key(), key(), integer(), integer()) -> binary() | nil.

-spec del      (server(), key())    -> ok.
-spec exists   (server(), key())    -> boolean().
-spec get      (server(), key())    -> string_value() | [binary()].
-spec hgetall  (server(), key())    -> [{binary(), binary()}]. 
-spec hkeys    (server(), key())    -> [binary()] | nil.
-spec incr     (server(), key())    -> integer().
-spec keys     (server(), key())    -> [binary()].
-spec llen     (server(), key())    -> non_neg_integer().
-spec mget     (server(), [key()])  -> [string_value()].
-spec scard    (server(), key())    -> integer().
-spec type     (server(), key())    -> atom().

-spec srandmember (server(), key()) -> binary() | nil.
-spec smembers (server(), key())  -> [binary()] | nil.
-spec sdiff    (server(), [key(), ...]) -> [binary()].
-spec sdiffstore (server(), key(), [key(), ...]) -> integer().

-type sort_option() :: {limit, non_neg_integer(), pos_integer()} | alpha.
-spec sort     (server(), key(), [sort_option()])      -> [binary()] | nil.

-spec srandmember (server(), key(), integer())         -> binary() | nil.
-spec expire   (server(), key(),   integer())          -> boolean().
-spec hdel     (server(), key(),   value())            -> ok.
-spec hget     (server(), key(),   key())              -> string_value().
-spec hmget    (server(), key(),   [key()])            -> [string_value()].
-spec hmset    (server(), key(),   [{key(), value()}]) -> ok | nil.
-spec incrby   (server(), key(),   value())            -> integer().
-spec lpush    (server(), key(),   value())            -> integer().
-spec publish  (server(), value(), value())            -> ok.
-spec rpush    (server(), key(),   value())            -> integer().
-spec sadd     (server(), key(),   value())            -> ok.
-spec saddcount(server(), key(),   value())            -> binary().
-spec set      (server(), key(),   value())            -> ok | nil.
-spec setnx    (server(), key(),   value())            -> boolean().
-spec sismember(server(), key(),   value())            -> boolean().
-spec sunion   (server(), [key()])                     -> [binary()].
-spec sunionstore(server(), key(), [key(), ...])       -> ok.
-spec srem     (server(), key(),   value())            -> ok.
-spec smove    (server(), key(),   key(),   value())   -> ok.
-spec zadd     (server(), key(),   value(), integer()) -> ok.
-spec zrem     (server(), key(),   value())            -> ok.
-spec zrangebyscore (server(), key(), integer(), integer(), [sort_option()]) -> [binary()] | nil.
-spec zrangebyscore (server(), key(), integer(), integer()) -> [binary()] | nil.

-type bit() :: 0 | 1.
-spec getbit   (server(), key(),   integer()       )   -> bit().
-spec setbit   (server(), key(),   integer(), bit())   -> ok.

-spec hincrby  (server(), key(), key(),     value())   -> integer().
-spec hset     (server(), key(), key(),     value())   -> ok.
-spec hsetnx   (server(), key(), key(),     value())   -> boolean().
-spec lrange   (server(), key(), integer(), integer()) -> list(binary()).
-spec lrem     (server(), key(), integer(), value())   -> ok.
-spec ltrim    (server(), key(), integer(), integer()) -> ok.

-spec eval(server(), iolist()) -> any().
-spec eval(server(), iolist(), [key()], [any()]) -> any().


connect(Host, Port)        -> eredis:start_link(Host, Port).
quit(Pid) when is_pid(Pid) -> eredis:stop(Pid).

call(Pid, Cmd)  -> eredis:q(Pid, Cmd).
pipe(Pid, Cmd)  -> eredis:qp(Pid, Cmd).
exec(Pid, Cmd)  -> eredis:q_no_reply(Pid, Cmd).
    

reply(Type, Cmd) ->
    try redis_cxn:exec(Type, Cmd) of
        {error, no_connection} = Error -> log_exec_error(Cmd, Error);
        {error, _Other}        = Error -> log_exec_error(Cmd, Error);
        Reply                          -> fetch(Cmd, Reply)
    catch
        Class:Type -> log_exec_error(Cmd, {caught_exception, Class, Type})
    end.

log_exec_error(Cmd, Error) ->
    error_logger:error_msg("~p failed with ~p on cmd ~p, details:~p~n", [?MODULE, Error, Cmd, erlang:get_stacktrace()]),
    {redis_error, Error}.

brpoplpush(Pid,  S, D, T1, T2) when is_pid(Pid) ->
    Cmd = ["BRPOPLPUSH", S, D, T1],
    fetch(Cmd, eredis:q(Pid, Cmd, T2)).

%% sort
sort(Server, Key, Options) ->
    ParsedOptions =
        lists:flatmap(
            fun({limit, Offset, Count}) ->
                ["LIMIT", integer_to_list(Offset), integer_to_list(Count)];
                (alpha) -> ["ALPHA"]
            end, Options),
    reply(Server, ["SORT", Key | ParsedOptions]).

%% 2 args
del         (Server, Key)  -> reply(Server, ["DEL",         Key  ]).
exists      (Server, Key)  -> reply(Server, ["EXISTS",      Key  ]).
get         (Server, Key)  -> reply(Server, ["GET",         Key  ]).
hgetall     (Server, Hash) -> reply(Server, ["HGETALL",     Hash ]).
hkeys       (Server, Hash) -> reply(Server, ["HKEYS",       Hash ]).
incr        (Server, Key)  -> reply(Server, ["INCR",        Key  ]).
keys        (Server, Patt) -> reply(Server, ["KEYS",        Patt ]).
llen        (Server, Key)  -> reply(Server, ["LLEN",        Key  ]).
scard       (Server, Key)  -> reply(Server, ["SCARD",       Key  ]).
smembers    (Server, Key)  -> reply(Server, ["SMEMBERS",    Key  ]).
srandmember (Server, Key)  -> reply(Server, ["SRANDMEMBER", Key  ]).
type        (Server, Key)  -> reply(Server, ["TYPE",        Key  ]).

mdel        (Server, Keys) -> reply(Server, ["DEL"    |     Keys ]).
mget        (Server, Keys) -> reply(Server, ["MGET"   |     Keys ]).
sunion      (Server, Keys) -> reply(Server, ["SUNION" |     Keys ]).

sdiff      (Server, [_First_Set | _Subtract_Sets] = Sets)       -> reply(Server, ["SDIFF" | Sets]).
sdiffstore (Server, Dest, [_First_Set | _Subtract_Sets] = Sets) -> reply(Server, ["SDIFFSTORE", Dest | Sets]).

%% 3 args
sadd        (Server, Key,    Value)    -> _ = saddcount(Server, Key, Value), ok.

expire      (Server, Key,    Seconds)  -> reply(Server, ["EXPIRE",      Key,    Seconds  ]).
hdel        (Server, Key,    Value)    -> reply(Server, ["HDEL",        Key,    Value    ]).
hget        (Server, Prefix, Suffix)   -> reply(Server, ["HGET",        Prefix, Suffix   ]).
hmget       (Server, Key,    Keys)     -> reply(Server, ["HMGET",       Key,    Keys     ]).
incrby      (Server, Key,    Value)    -> reply(Server, ["INCRBY",      Key,    Value    ]).
lpush       (Server, Key,    Value)    -> reply(Server, ["LPUSH",       Key,    Value    ]).
publish     (Server, Chan,   Value)    -> reply(Server, ["PUBLISH",     Chan,   Value    ]).
rpush       (Server, Key,    Value)    -> reply(Server, ["RPUSH",       Key,    Value    ]).
saddcount   (Server, Key,    Value)    -> reply(Server, ["SADD",        Key,    Value    ]).
set         (Server, Key,    Value)    -> reply(Server, ["SET",         Key,    Value    ]).
setnx       (Server, Key,    Value)    -> reply(Server, ["SETNX",       Key,    Value    ]).
sismember   (Server, Key,    Value)    -> reply(Server, ["SISMEMBER",   Key,    Value    ]).
srandmember (Server, Key,    Count)    -> reply(Server, ["SRANDMEMBER", Key,    Count    ]).
srem        (Server, Key,    Value)    -> reply(Server, ["SREM",        Key,    Value    ]).
zrem        (Server, Key,    Value)    -> reply(Server, ["ZREM",        Key,    Value    ]).

hmset       (Server, Key,    KV_Pairs) -> reply(Server, ["HMSET",       Key  | flatten(KV_Pairs) ]).
sadd_multi  (Server, Key,    Value)    -> reply(Server, ["SADD",        Key  | Value             ]).
sunionstore (Server, Dest,   Keys)     -> reply(Server, ["SUNIONSTORE", Dest | Keys              ]).


getbit   (Server, Key, Feature)        -> reply(Server, ["GETBIT", Key, Feature       ]).
setbit   (Server, Key, Feature, Value) -> reply(Server, ["SETBIT", Key, Feature, Value]).


%% 4+ args
hset     (Server, Key, Field, Value) -> reply(Server, ["HSET",    Key, Field, Value ]).
hsetnx   (Server, Key, Field, Value) -> reply(Server, ["HSETNX",  Key, Field, Value ]).
hincrby  (Server, Key, Field, Value) -> reply(Server, ["HINCRBY", Key, Field, Value ]).
lrange   (Server, Key, Start, Stop)  -> reply(Server, ["LRANGE",  Key, Start, Stop  ]).
lrem     (Server, Key, Count, Value) -> reply(Server, ["LREM",    Key, Count, Value ]).
ltrim    (Server, Key, Min,   Max)   -> reply(Server, ["LTRIM",   Key, Min,   Max   ]).
smove    (Server, Src, Dest,  Value) -> reply(Server, ["SMOVE",   Src, Dest,  Value ]).
zadd     (Server, Key, Value, Score) -> reply(Server, ["ZADD",    Key, Score, Value ]).

zrangebyscore (Server, Key, Min, Max)          -> zrangebyscore(Server, Key, Min, Max, [limit, 0, 100]).
zrangebyscore (Server, Key, Min, Max, Options) -> reply(Server, ["ZRANGEBYSCORE", Key, Min, Max | Options]).

%% Scripting commands
eval(Server, Script)             -> eval(Server, Script, [], []).
eval(Server, Script, Keys, Args) -> reply(Server, ["EVAL", Script, length(Keys)] ++ Keys ++ Args).
    

%% ------------------------------------
%%  Internal support functions
%% ------------------------------------

fetch(["TYPE"        | _Rest] =  Cmd, Val)   -> make_atom(Cmd, Val);

fetch(["ADD"         | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["DEL"         | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["DELKEYS"     | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["GETBIT"      | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["HDEL"        | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["INCR"        | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["INCRBY"      | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["HINCRBY"     | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["LLEN"        | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["LPUSH"       | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["LREM"        | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["PUBLISH"     | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["RPUSH"       | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["SETBIT"      | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["SREM"        | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["SMOVE"       | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["SUNIONSTORE" | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["SCARD"       | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["SDIFFSTORE"  | _Rest] =  Cmd, Val)   -> make_int (Cmd, Val);
fetch(["ZREM"        | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);

fetch(["EXISTS"      | _Rest] =  Cmd, Val)   -> make_bool(Cmd, Val);
fetch(["EXPIRE"      | _Rest] =  Cmd, Val)   -> make_bool(Cmd, Val);
fetch(["HSET"        | _Rest] =  Cmd, Val)   -> make_ok  (Cmd, Val);
fetch(["SETNX"       | _Rest] =  Cmd, Val)   -> make_bool(Cmd, Val);
fetch(["HSETNX"      | _Rest] =  Cmd, Val)   -> make_bool(Cmd, Val);
fetch(["SISMEMBER"   | _Rest] =  Cmd, Val)   -> make_bool(Cmd, Val);
fetch(["HGETALL"     | _Rest] =  Cmd, Val)   -> make_kvpair(Cmd, Val); 

fetch(["LRANGE"      | _Rest] = _Cmd, nil)   -> [];
fetch(["LRANGE"      | _Rest] = _Cmd, Items) -> Items;

fetch(_Cmd, {ok, undefined}) -> nil;
fetch(_Cmd, {ok, <<"OK">>})  -> ok;
fetch(_Cmd, {ok, Reply})     -> Reply;
fetch( Cmd, Error)           -> log(Cmd, Error, nil).

make_atom(_Cmd, {ok, Bin}) -> binary_to_atom(Bin, utf8).

make_bool(_Cmd, {ok,   <<"1">>}) -> true;
make_bool(_Cmd, {ok,   <<"0">>}) -> false;
make_bool(_Cmd, {ok, undefined}) -> false;
make_bool( Cmd,           Error) -> log(Cmd, Error, false).

make_ok(_Cmd, {ok, _}) -> ok;
make_ok( Cmd,   Error) -> log(Cmd, Error, ok).

make_int(_Cmd, {ok, undefined}) -> -1;
make_int( Cmd, {ok, Bin}) when is_binary(Bin) ->
    try list_to_integer(binary_to_list(Bin))
    catch Err:Type -> log(Cmd, {error, {invalid_integer, {Err, Type}}}, -1)
    end;
make_int( Cmd, Error) -> log(Cmd, Error, -1).

make_kvpair(_Cmd, {ok,   []}) -> [];
make_kvpair(_Cmd, {ok, List}) -> pair(List, []).

pair([],         L) -> lists:reverse(L);
pair([A,B|Rest], L) -> pair(Rest, [{A,B}|L]). 

log(Cmd, Error, Return_Value) -> error_logger:error_msg("~p cmd ~p rcvd error: ~p~n", [?MODULE, Cmd, Error]), 
                                 Return_Value.

flatten(KV_Pairs) -> lists:foldr(fun({K, V}, Acc) -> [K, V|Acc] end, [], KV_Pairs).
