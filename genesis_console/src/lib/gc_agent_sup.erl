-module(gc_agent_sup).
-behavior(supervisor).

-export([start_link/0, add_sub/1]).
-export([init/1]).

-include("common.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  AgentSpec = get_agent_spec(),
  MonitorSpec = [{gc_monitor, {gc_monitor, start_link, []}, 
      permanent, 1000, worker, [gc_monitor]}],

  {ok, {{one_for_one, 1, 1}, AgentSpec ++ MonitorSpec}}.

add_sub(R = #tab_agent{}) ->
  [AgentSpec] = spec([R], []),
  supervisor:start_child(?MODULE, AgentSpec).

%%%
%%% Private
%%%

get_agent_spec() ->
  spec(gc_db:get_all(tab_agent), []).

spec([], L) -> L;
spec([H = #tab_agent{}|T], L) ->
  NL = L ++ [{erlang:make_ref(), {gc_agent, start_link, [H]},
      permanent, 1000, worker, []}],
  spec(T, NL).
