
%% -compile([{parse_transform, lager_transform}]).

-define(TT_DEBUG_MSG(Msg, Args), lager:debug(Msg, Args)).
-define(TT_INFO_MSG(Msg, Args),  lager:info(Msg, Args)).
-define(TT_WARN_MSG(Msg, Args),  lager:warning(Msg, Args)).
-define(TT_ERROR_MSG(Msg, Args), lager:error(Msg, Args)).
