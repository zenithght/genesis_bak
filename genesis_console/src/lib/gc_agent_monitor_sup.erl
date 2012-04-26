-module(gc_agent_monitor_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("common.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Spec = [
    {gc_agent_monitor, {gc_agent_monitor, start_link, []}, 
      permanent, 1000, worker, [gc_agent_monitor]}
  ] ++ spec(gc_db:get_all(tab_agent), []),
  {ok, {{one_for_one, 1, 1}, Spec}}.

spec([], L) -> L;
spec([H = #tab_agent{identity = Id}|T], L) ->
  Name = erlang:list_to_atom("gc_" ++ Id ++ "_agent"),
  NL = L ++ [{Name, {gc_agent, start_link, [H, Name]},
      permanent, 1000, worker, [Name]}],
  spec(T, NL).
