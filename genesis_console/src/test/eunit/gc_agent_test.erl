-module(gc_agent_test).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

init_test_() -> ?SPAWN_TEST([
  ]).

setup() ->
  Root = #tab_agent{aid = 1, identity = "root", level = ?GC_ROOT_LEVEL, parent = "root", cash = 0, credit = 0},
  Lv1 = #tab_agent{aid = 2, identity = "lv1", level = 1, parent = "root", cash = 10, credit = 10},

  meck:new(gc_db),
  meck:expect(gc_db, get_all, 
    fun(tab_agent) -> [Root, Lv1] end),
  meck:expect(gc_db, init_xref, 
    fun(_Type, _Agent) -> ok end),
  meck:expect(gc_db, get_collect_list, 
    fun(1) -> [2]; 
       (2) -> [] 
    end),
  meck:expect(gc_db, get_turnover,
    fun(week, 2) -> [{date(), 10}, {date(), 20}]; 
       (week, _Agent) -> [];
       (today, 2) -> 10;
       (today, _Agent) -> 0
    end),

  ?assertMatch({ok, _}, gc_agent_sup:start_link()),
  ?assertMatch(ok, gc_agent:collect()).

cleanup(_) ->
  meck:unload(gc_db).
