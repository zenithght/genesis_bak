-module(gc_agent).
-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([collect/0]).
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

  {ok, #gc_agent{
      balance = R#tab_agent.cash + R#tab_agent.credit,

      cash = R#tab_agent.cash,
      credit = R#tab_agent.credit,

      week_turnover = WeekTurnover,
      today_turnover = TodayTurnover,

      today_collect_turnover = ?UNDEF,
      week_collect_turnover = ?UNDEF
    }}.

terminate(Reason, _R) ->
  ok.

handle_cast(collect, R = #gc_agent{collect_timer = T}) when is_reference(T) ->
  {noreply, R};

handle_cast(collect, R = #gc_agent{}) ->
  case gen_collect_list(R#gc_agent.aid) of
    [] ->
      gen_server:cast(report, self()),
      {noreply, R};
    L ->
      T = erlang:start_timer(?GC_COLLECT_TIME, self(), collect),
      {noreply, R#gc_agent{collect_list = L, collect_timer = T}}
  end;

handle_cast(report, R = #gc_agent{level = L}) when L =:= ?GC_ROOT_LEVEL ->
  {noreply, R};
handle_cast(report, R = #gc_agent{}) ->
  gen_server:cast({report, #agt{}}, R#gc_agent.parent),
  {noreply, R};

handle_cast({report, Agt = #agt{}}, R) ->
  TodayCollectTurnover = R#gc_agent.today_collect_turnover + Agt#agt.today_turnover,
  WeekCollectTurnover = R#gc_agent.week_collect_turnover + Agt#agt.week_turnover,
  gc_db:update(Agt),
  {noreply, R};

handle_cast(stop, _R) ->
  {stop, normal, _R}.

handle_call(detail, _From, R = #gc_agent{}) ->
  {reply, R, R}.

%%%
%%% Client Function
%%%

collect() ->
  gen_server:cast(collect, whereis(gc_root_agent)).

%%%
%%% Private Function
%%%

gen_collect_list(Id) ->
  L = gc_db:get_collect_list(Id),
  lists:map(fun(I) -> {I, false} end, L).
