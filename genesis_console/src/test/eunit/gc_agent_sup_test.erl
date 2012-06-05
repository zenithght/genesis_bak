-module(gc_agent_sup_test).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

gc_agent_sup_test_() -> ?SPAWN_TEST([
      ?_assert(is_pid(whereis(gc_monitor))),
      ?_assert(is_pid(whereis(gc_root_agent)))
  ]).

setup() ->
  meck:new(gc_db),
  meck:expect(gc_db, get_all, fun(tab_agent) -> [] end),
  meck:expect(gc_db, get_turnover, 
    fun(_Agent) -> [{?DATE, 10}] end),
  meck:expect(gc_db, get_balance, fun(_Agent) -> 1000 end),
  meck:expect(gc_db, monitor, fun (_) -> ok end),

  gc_monitor:start_link(),
  gc_agent:start_link(#tab_agent{identity = root}).

cleanup(_) ->
  gc_monitor:stop(),
  gc_agent:stop(root),
  meck:unload(gc_db).
