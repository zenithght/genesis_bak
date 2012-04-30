-module(gc_agent).
-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-behavior(gen_server).

-include("common.hrl").

start_link(R = #tab_agent{identity = Id}) ->
  gen_server:start_link({local, ?GC_AGENT_NAME(Id)}, ?MODULE, [R], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Callback
%%

init([R = #tab_agent{}]) ->
  ok = gc_db:init_xref(agent, R#tab_agent.aid),
  ok = gc_db:init_xref(player, R#tab_agent.aid),

  WeekTurnover = gc_db:get_turnover(week, R#tab_agent.aid),
  TodayTurnover = gc_db:get_turnover(today, R#tab_agent.aid),

  Balance = gc_db:get_balance(R#tab_agent.aid),
  
  {ok, #gc_agent{
      balance = Balance,

      cash = R#tab_agent.cash,
      credit = R#tab_agent.credit,

      week_turnover = WeekTurnover,
      today_turnover = TodayTurnover,

      today_collect_turnover = ?UNDEF,
      week_collect_turnover = ?UNDEF
    }}.

terminate(Reason, _LoopData) ->
  ok.

handle_cast(stop, _LoopData) ->
  {stop, normal, _LoopData}.

handle_call(detail, _From, L = #gc_agent{}) ->
  {reply, L, L}.
