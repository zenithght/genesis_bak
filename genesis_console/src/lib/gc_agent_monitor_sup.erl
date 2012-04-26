-module(gc_agent_monitor_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_all, 1, 1}, 
      [
        {gc_agent_monitor, {gc_agent_monitor, start_link, []}, 
          permanent, 1000, worker, [gc_agent_monitor]}
      ]
    }}.
