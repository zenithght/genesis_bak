-define(UNDEF, undefined).

-define(WAIT_TABLE, 10 * 1000).
-define(CONNECT_TIMEOUT, 2000).

%%% Error codes

-define(LOG(L), error_logger:info_report([{debug, {?MODULE, ?LINE, self()}}] ++ L)).
-define(ERROR(L), error_logger:error_report([{debug, {?MODULE, ?LINE, self()}}] ++ L)).

