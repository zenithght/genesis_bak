-module(genesis_01_news).

-export([init/0, stop/1]).

-include("../../src/lib/schema.hrl").

% This script is first executed at server startup and should
% return a list of WatchIDs that should be cancelled in the stop
% function below (stop is executed if the script is ever reloaded).
init() ->
  schema:install(),
  server:start("127.0.0.1", 8002),

  case db:index_read(tab_admin, <<"root">>, #tab_admin.username) of
    [_] ->
      ok;
    _ ->
      Admin = #tab_admin { id = 1, username = <<"root">>, password = <<"password">>, root = 1 },
      db:write(Admin),
      error_logger:info_report("INIT ROOT USER")
  end,

  {ok, []}.

stop(ListOfWatchIDs) ->
    lists:map(fun boss_news:cancel_watch/1, ListOfWatchIDs).
