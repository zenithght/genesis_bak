-define(L(Msg, Params), io:format(Msg ++ "~n", Params)).
-define(LOG(L), error_logger:info_report([{debug, {?MODULE, ?LINE, self()}}] ++ L)).
-define(ERROR(L), error_logger:error_report([{{module, ?MODULE}, {line, ?LINE}, {self, self()}},L])).

-define(WAIT_TABLE, 10 * 1000).

-define(MAX_RAISES, 3).
-define(MAX_PLAYERS, 500000).

-define(GAME_SERVERS, 'GAME SERVERS').
-define(MULTIBOTS, 'MULTIBOTS').
-define(LAUNCHERS, 'LAUNCHERS').

-define(UNDEF, undefined).

%%% Error codes

-define(ERR_UNKNOWN, 0).
-define(ERR_BAD_LOGIN, 1).
-define(ERR_ACCOUNT_DISABLED, 2).
-define(ERR_START_DISABLED, 3).
