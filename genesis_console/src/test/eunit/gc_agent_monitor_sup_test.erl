-module(gc_agent_monitor_sup_test).
-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").

start_link_test() ->
  meck:new(gc_db),
  meck:expect(gc_db, get_all, fun(tab_agent) -> [#tab_agent{identity = "root"}] end),
  gc_agent_sup:start_link(),
  timer:sleep(1000),
  ?assert(is_pid(whereis(gc_monitor))),
  ?assert(is_pid(whereis(gc_agent_sup))),
  ?assert(is_pid(whereis(gc_root_agent))),
  meck:unload(gc_db).
