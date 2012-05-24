-module(gc_db).
-export([start/0, get_all/1, get_turnover/2, lookup_agent/1]).

-export([monitor/1]).

-include("common.hrl").

start() ->
  mnesia:start().

get_all(_Table) ->
  [].

lookup_agent(PId) ->
  [Ref] = mnesia:dirty_read(tab_agent_player, PId),
  Ref#tab_agent_player.aid.

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

monitor(Table) when is_atom(Table) ->
  {ok, _} = mnesia:subscribe({table, Table, simple}).
