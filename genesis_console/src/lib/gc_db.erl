-module(gc_db).
-export([start/0, get_all/1, get_turnover/2]).

-include("common.hrl").

start() ->
  mnesia:start().

get_all(tab_agent) ->
  [].

get_daily_turnover(Agent) ->
  get_daily_turnover(Agent, 10).

get_daily_turnover(Agent, Days) ->
  [{{y, m, d}, turnover}].

%% 获取代理当周的流水数据（非收集）
get_turnover(week, Id) ->
  ok;

%% 获取代理当日的流水数据（非收集）
get_turnover(today, Id) ->
  ok.
