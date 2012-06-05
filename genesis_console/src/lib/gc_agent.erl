-module(gc_agent).
-behavior(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([collect/0, detail/1, to_pid/1, log_turnover/3]).

-include("common.hrl").

start_link(S = #tab_agent{identity = Id}) ->
  gen_server:start_link({local, to_pid(Id)}, ?MODULE, [S], []).

stop(Identity) ->
  gen_server:cast(to_pid(Identity), stop).

%%
%% Callback
%%

init([S = #tab_agent{}]) ->
  Agent = #gc_agent{
      id = S#tab_agent.aid,
      identity = S#tab_agent.identity,
      level = S#tab_agent.level,
      cash = S#tab_agent.cash,
      credit = S#tab_agent.credit,
      balance = S#tab_agent.cash + S#tab_agent.credit,
      parent = S#tab_agent.parent,
      turnover_daily = gc_db:get_turnover(S#tab_agent.aid),
      clct_table = ets:new(clct_table, [set, {keypos, #agt.id}])
    },

  Sum = compute_sum_data(Agent),

  {ok, Agent#gc_agent{sum = Sum}}.

terminate(_, _) ->
  ok.

%% 通知进程向下级代理进程发送“发送汇总数据”的消息。
%% 此消息从ROOT始发，以递归形式向下级发送。

handle_cast(collect, S) when is_reference(S#gc_agent.clct_timer) ->
  %% 收集数据时不响应任何collect消息
  {noreply, S};
handle_cast(collect, S = #gc_agent{}) ->
  case gc_db:get_collection_list(S#gc_agent.identity) of
    [] ->
      gen_server:cast(self(), report),
      {noreply, S};
    L ->
      T = erlang:start_timer(?GC_COLLECT_TIME, self(), collect),
      lists:map(fun ({Identity}) ->
            gen_server:cast(gc_agent:to_pid(Identity), collect) end, L),
      {noreply, S#gc_agent{clct_list = L, clct_timer = T}}
  end;

%% 通知进程向上级代理进程发送汇总数据
handle_cast(report, S = #gc_agent{level = ?GC_ROOT_LEVEL}) ->
  Sum = compute_sum_data(S#gc_agent{}),
  {noreply, S#gc_agent{sum = Sum}};
handle_cast(report, S = #gc_agent{}) ->
  Sum = compute_sum_data(S#gc_agent{}),
  Name = gc_agent:to_pid(S#gc_agent.parent),
  gen_server:cast(Name, {report, Sum}),
  {noreply, S#gc_agent{sum = Sum}};

%% 接收下级代理进程发送的汇总数据
handle_cast({report, #agt{}}, S) 
  when not is_reference(S#gc_agent.clct_timer) ->
    {noreply, S};
handle_cast({report, Agt = #agt{identity = Identity}}, S = #gc_agent{clct_list = L}) ->
  case is_include(Identity, L) of
    false -> 
      {noreply, S};
    true ->
      ets:insert(S#gc_agent.clct_table, Agt),
      case exclude(Identity, L) of
        empty ->
          gen_server:cast(self(), report),
          {noreply, S#gc_agent{clct_list = [], clct_timer = ?UNDEF}};
        EL ->
          {noreply, S#gc_agent{clct_list = EL}}
      end
  end;

handle_cast({turnover, Date, Turnover}, S = #gc_agent{turnover_daily = Daily}) ->
  {noreply, S#gc_agent{turnover_daily = Daily ++ [{Date, Turnover}]}};

handle_cast(stop, _S) ->
  {stop, normal, _S}.

handle_call(detail, _From, S = #gc_agent{sum = Sum}) ->
  Result = [
    {identity, S#gc_agent.identity},
    {credit, S#gc_agent.credit}, {cash, S#gc_agent.cash}, {balance, Sum#agt.balance}, 
    {today_turnover, Sum#agt.today_turnover}, {week_turnover, Sum#agt.week_turnover}],

  {reply, Result, S}.

%%%
%%% Client Function
%%%

to_pid(Identity) when is_atom(Identity) ->
  list_to_atom("gc_" ++ atom_to_list(Identity) ++ "_agent").

collect() ->
  gen_server:cast(whereis(gc_root_agent), collect).

detail(Identity) ->
  Name = "gc_" ++ atom_to_list(Identity) ++ "_agent",
  Agent = whereis(list_to_existing_atom(Name)),
  gen_server:call(Agent, detail).

log_turnover(Identity, Date, Turnover) ->
  gen_server:cast(to_pid(Identity), {turnover, Date, Turnover}).

%%%
%%% Private Function
%%%

collect_agt_sum(S = #gc_agent{}) ->
  #agt{}.

today_sum(L) ->
  day_sum(L, date()).

week_sum(L) ->
  week_sum(L, 7, 0).

week_sum(L, 0, Sum) -> 
  Sum + day_sum(L, ?DATE);
week_sum(L, N, Sum) ->
  S = Sum + day_sum(L, ?DATE(0 - N)),
  week_sum(L, N - 1, S).

day_sum(L, D) ->
  lists:sum(proplists:append_values(D, L)).

update_agt_sum(Agt = #agt{id = Id}, {WL, CL}) ->
  case lists:keyfind(Id, WL) of
    false ->
      %% TODO log this error
      {WL, CL};
    Reporter ->
      gc_db:update(Agt),
      {lists:keydelete(Id, WL), CL ++ [Reporter]}
  end.

compute_sum_data(S = #gc_agent{turnover_daily = Turnover}) ->
  TodayTurnover = today_sum(Turnover),
  WeekTurnover = week_sum(Turnover),

  Acc0 = {S#gc_agent.balance, TodayTurnover, WeekTurnover},

  Fun = fun (A = #agt{}, {Balance, Today, Week}) ->
      {Balance + A#agt.balance, Today + A#agt.today_turnover, Week + A#agt.week_turnover} end,

  {Balance, Today, Week} = ets:foldl(Fun, Acc0, S#gc_agent.clct_table),

  #agt{
    id = S#gc_agent.id, identity = S#gc_agent.identity,
    cash = S#gc_agent.cash, credit = S#gc_agent.cash, balance = Balance,
    today_turnover = Today, week_turnover = Week, update_time = {date(), time()}
  }.

is_include(Identity, List) ->
  case lists:keyfind(Identity, 1, List) of
    false -> false;
    _ -> true
  end.

exclude(Identity, List) ->
  case lists:keydelete(Identity, 1, List) of
    [] -> empty;
    L -> L
  end.

%%%
%%% Unit Test
%%% 

-include_lib("eunit/include/eunit.hrl").

sum_test() ->
  ?assertEqual(20, today_sum([{date(), 1}, {date(), 9}, {date(), 10}])),
  ?assertEqual(10, today_sum([{date(), 1}, {date(), 9}, {{2000, 1, 1}, 10}])),
  ?assertEqual(10, week_sum([{date(), 1}, {?DATE(-1), 2}, {?DATE(-2), 7}])),
  ?assertEqual(10, week_sum([{date(), 1}, {?DATE(-8), 1}, {?DATE(-1), 2}, {?DATE(-2), 7}])).

to_pid_test() ->
  ?assertEqual(gc_test_agent, gc_agent:to_pid(test)).
