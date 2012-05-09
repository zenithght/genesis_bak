-module(gc_agent_test).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ROOT_ID, 1).
-define(LV1_ID, 2).

init_test_() -> ?SPAWN_TEST([
      fun () -> 
          ?SLEEP,
          ?assertMatch(
            [{identity, root}, {credit, _}, {cash, _}, {balance, _}, {today_turnover, _}, {week_turnover, _}], 
            gc_agent:detail(root))
      end
    ]).

setup() ->
  Root = #tab_agent{aid = ?ROOT_ID, identity = root, level = ?GC_ROOT_LEVEL, parent = root, cash = 0, credit = 0},
  Lv1 = #tab_agent{aid = ?LV1_ID, identity = lv1, level = 1, parent = root, cash = 10, credit = 10},

  meck:new(gc_db),
  lists:map(fun({N, F}) -> meck:expect(gc_db, N, F) end, 
    [
      {get_all, fun (tab_agent) -> [Root, Lv1] end},

      {init_xref, fun (player, _Agent) -> ok;
                      (agent, _Agent) -> ok end},

      {get_sub_list, fun (?ROOT_ID) -> [?LV1_ID]; 
                         (?LV1_ID) -> [] end},

      {get_turnover, fun (week, ?LV1_ID) -> 10;
                         (week, _Agent) -> 0;
                         (today, ?LV1_ID) -> 10;
                         (today, _Agent) -> 0 end}
    ]),

  ?assertMatch({ok, _}, gc_agent_sup:start_link()),
  ?assertMatch(ok, gc_agent:collect()).

cleanup(_) ->
  meck:unload(gc_db).
