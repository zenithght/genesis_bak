-module(gc_db).
-export([start/0, get_all/1, get_turnover/2, lookup_agent/1, get_collection_list/1]).

-export([monitor/1]).

-include("common.hrl").

start() ->
  mnesia:start().

get_all(Table) ->
  Fun = fun() -> mnesia:foldl(fun (R, Acc) -> Acc ++ [R] end, [], Table) end,
  {atomic, R} = mnesia:transaction(Fun),
  R.

get_collection_list(Identity) ->
  {atomic, R} = mnesia:transaction(fun () ->
        Match = #tab_agent{identity = '$1', parent = Identity, _ = '_'},
        Result = '$1',
        mnesia:select(tab_agent,[{Match, [], [Result]}])
    end),
  [{N} || N <- R].

lookup_agent(PId) ->
  [Ref] = mnesia:dirty_read(tab_agent_player, PId),
  Ref#tab_agent_player.aid.

get_daily_turnover(Agent) ->
  get_daily_turnover(Agent, 10).

get_daily_turnover(Agent, Days) ->
  [{{y, m, d}, turnover}].

get_turnover(week, Id) ->
  ok;

%% 获取代理当日的流水数据（非收集）
get_turnover(today, Id) ->
  ok.

monitor(Table) when is_atom(Table) ->
  {ok, _} = mnesia:subscribe({table, Table, simple}).
