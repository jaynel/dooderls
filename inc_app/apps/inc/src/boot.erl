-module(boot).

-export([strap/0]).

strap() ->
    application:start(appmon),
    application:start(gs),
    application:start(inc).
