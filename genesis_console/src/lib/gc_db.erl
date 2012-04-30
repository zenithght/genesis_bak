-module(gc_db).
-export([start/0]).

-include("common.hrl").

start() ->
  mnesia:start().

get_daily_turnover(Agent) ->
  get_daily_turnover(Agent, 10).

get_daily_turnover(Agent, Days) ->
  [{{y, m, d}, turnover}].
