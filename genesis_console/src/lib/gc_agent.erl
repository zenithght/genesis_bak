-module(gc_agent).
-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([collect/0]).
-behavior(gen_server).

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

  WeekTurnover = gc_db:get_turnover(week, S#tab_agent.aid),
  TodayTurnover = gc_db:get_turnover(today, S#tab_agent.aid),

  {ok, #gc_agent{
      balance = S#tab_agent.cash + S#tab_agent.credit,

      cash = S#tab_agent.cash,
      credit = S#tab_agent.credit,

      week_turnover = WeekTurnover,
      today_turnover = TodayTurnover,

      today_collect_turnover = ?UNDEF,
      week_collect_turnover = ?UNDEF
    }}.

terminate(Season, _S) ->
  ok.

handle_cast(collect, S) when is_reference(S#gc_agent.clct_t) ->
  {noreply, S};
handle_cast(collect, S = #gc_agent{}) ->
  case gen_clct_l(S#gc_agent.aid) of
    [] ->
      gen_server:cast(report, self()),
      {noreply, S};
    L ->
      T = erlang:start_timer(?GC_COLLECT_TIME, self(), collect),
      {noreply, S#gc_agent{clct_l = L, clct_t = T}}
  end;

handle_cast(report, S = #gc_agent{level = L}) when L =:= ?GC_ROOT_LEVEL ->
  {noreply, S};
handle_cast(report, S = #gc_agent{}) ->
  gen_server:cast(#agt{}, S#gc_agent.parent),
  {noreply, S};

handle_cast(#agt{}, S) when not is_reference(S#gc_agent.clct_t) ->
  {noreply, S};
handle_cast(A = #agt{}, S = #gc_agent{}) ->
  L = {WL, CL} = update_agt_sum(A, S#gc_agent.clct_l),
  case WL of
    [] ->
      compute_collect_data(S#gc_agent.aid),
      gen_server:cast(report, self());
    _ -> ok
  end,
  {noreply, S#gc_agent{clct_l = L}};

handle_cast(stop, _S) ->
  {stop, normal, _S}.

handle_call(detail, _From, S = #gc_agent{}) ->
  {reply, S, S}.

%%%
%%% Client Function
%%%

collect() ->
  gen_server:cast(collect, whereis(gc_root_agent)).

%%% Private Function
%%%

gen_clct_l(Id) ->
  {gc_db:get_clct_l(Id), []}.

%%% WL not collect list
%%% CL cllected list
update_agt_sum(Agt = #agt{id = Id}, {WL, CL}) ->
  case lists:keyfind(Id, WL) of
    false ->
      %% TODO log this error
      {WL, CL};
    Reporter ->
      gc_db:update(Agt),
      {lists:keydelete(Id, WL), CL ++ [Reporter]}
  end.

compute_collect_data(Id) ->
  ok.
