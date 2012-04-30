-module(gc_db).
-export([start/0]).

-include("common.hrl").

start() ->
  mnesia:start().

get_daily_turnover() ->
  get_daily_turnover(10).

get_daily_turnover(Days) ->
  [{{y, m, d}, turnover}].
