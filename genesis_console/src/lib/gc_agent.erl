-module(gc_agent).
-behavior(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([collect/0, detail/1, to_pid/1]).

-include("common.hrl").

start_link(S = #tab_agent{identity = Id}) ->
  gen_server:start_link({local, ?GC_AGENT_NAME(Id)}, ?MODULE, [S], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Callback
%%

init([S = #tab_agent{}]) ->
  ok = gc_db:init_xref(agent, S#tab_agent.aid),
  ok = gc_db:init_xref(player, S#tab_agent.aid),

  Turnover = gc_db:get_turnover(S#tab_agent.aid),

  {ok, #gc_agent{
      id = S#tab_agent.aid,
      identity = S#tab_agent.identity,
      balance = S#tab_agent.cash + S#tab_agent.credit,
      parent = S#tab_agent.parent,

      cash = S#tab_agent.cash,
      credit = S#tab_agent.credit,

      turnover_daily = Turnover,
      today_turnover = today_sum(Turnover),
      week_turnover = week_sum(Turnover),

      clct_table_id = ets:new(clct_table, [])
    }}.

terminate(Season, _S) ->
  ok.

%% 通知进程向下级代理进程发送“发送汇总数据”的消息。
%% 此消息从ROOT始发，以递归形式向下级发送。
handle_cast(collect, S) when is_reference(S#gc_agent.clct_timer) ->                     
  %% 收集数据时不响应任何collect消息
  {noreply, S};
handle_cast(collect, S = #gc_agent{}) ->
  case gc_db:get_collection_list(S#gc_agent.id) of
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
  {noreply, S};
handle_cast(report, S = #gc_agent{}) ->
  Agt = compute_sum_data(S),
  Name = gc_agent:to_pid(S#gc_agent.parent),
  gen_server:cast(Name, {to_receive, Agt}),
  {noreply, S};

%% 接收下级代理进程发送的汇总数据
handle_cast({to_receive, #agt{}}, S) 
  when not is_reference(S#gc_agent.clct_timer) ->
    {noreply, S};
handle_cast({to_receive, A = #agt{}}, S = #gc_agent{}) ->
  %% update #agt to ets
  case lists:keyfind(A#agt.identity, 1, S#gc_agent.clct_list) of
    false ->
      {noreply, S};
    {Identity} ->
      ets:insert(S#gc_agent.clct_table_id, A),
      L = lists:keydelete(A#agt.identity, 1, S#gc_agent.clct_list),

      case L of
        [] ->
          Sum = compute_sum_data(S),
          gen_server:cast(self(), report),
          {noreply, S#gc_agent{sum = Sum, clct_list = [], clct_timer = ?UNDEF}};
        _ ->
          {noreply, S#gc_agent{clct_list = L}}
      end
  end;

handle_cast({turnover, Date, Turnover}, S) ->
  {noreply, S};

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

compute_sum_data(S = #gc_agent{}) ->
  Acc0 = {S#gc_agent.balance, S#gc_agent.today_turnover, S#gc_agent.week_turnover},

  Fun = fun (A = #agt{}, {Balance, Today, Week}) ->
      {Balance + A#agt.balance, Today + A#agt.today_turnover, Week + A#agt.week_turnover} end,

  {Balance, Today, Week} = ets:foldl(Fun, Acc0, S#gc_agent.clct_table_id),

  #agt{
    id = S#gc_agent.id,               %% 代理编号
    identity = S#gc_agent.identity,   %% 代理标识符
    cash = S#gc_agent.cash,             %% 现金
    credit = S#gc_agent.credit,           %% 信用额
    balance = Balance,          %% 账户余额 下级代理上报
    today_turnover = Today,   %% 当日流水 下级代理上报
    week_turnover = Week,    %% 当周流水 下级代理上报
    update_time = now()
  }.

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
