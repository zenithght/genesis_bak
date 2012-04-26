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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
  {ok, _} = gc_agent_monitor_sup:start_link(),
  timer:sleep(100),
  ?assert(is_pid(whereis(gc_agent_monitor))),
  ?assert(is_pid(whereis(gc_agent_monitor_sup))).

-endif.
