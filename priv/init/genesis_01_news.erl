-module(genesis_01_news).

-export([init/0, stop/1]).

% This script is first executed at server startup and should
% return a list of WatchIDs that should be cancelled in the stop
% function below (stop is executed if the script is ever reloaded).
init() ->
  agent:start(),
  {ok, []}.

stop(ListOfWatchIDs) ->
    lists:map(fun boss_news:cancel_watch/1, ListOfWatchIDs).
