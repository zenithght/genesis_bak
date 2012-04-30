-module(gc_db).
-export([start/0, get_all/1]).

-include("common.hrl").

start() ->
  mnesia:start().

get_all(tab_agent) ->
  [].

get_daily_turnover(Agent) ->
  get_daily_turnover(Agent, 10).

get_daily_turnover(Agent, Days) ->
  [{{y, m, d}, turnover}].
