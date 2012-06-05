-module(gc_agent_test).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ROOT_ID, 1).
-define(LV1A_ID, 2).
-define(LV1B_ID, 3).
-define(LV2A_ID, 4).

-define(ROOT_KEY, root).
-define(LV1A_KEY, lv1a).
-define(LV1B_KEY, lv1b).
-define(LV2A_KEY, lv2a).

init_test_() -> ?SPAWN_TEST([
      fun () -> 
          gc_agent:collect(),
          ?SLEEP,
          ?assertMatch([{identity, root}, {credit, _}, {cash, _}, {balance, _}, {today_turnover, 25}, {week_turnover, 25}], gc_agent:detail(root)),

          gc_agent:log_turnover(root, date(), 10),
          gc_agent:collect(),
          ?SLEEP,
          ?assertMatch([{identity, root}, {credit, _}, {cash, _}, {balance, _}, {today_turnover, 35}, {week_turnover, 35}], gc_agent:detail(root)),

          gc_agent:log_turnover(?LV1A_KEY, date(), 10),
          gc_agent:collect(),
          ?SLEEP,
          ?assertMatch([{identity, root}, {credit, _}, {cash, _}, {balance, _}, {today_turnover, 45}, {week_turnover, 45}], gc_agent:detail(root)),

          gc_agent:log_turnover(?LV2A_KEY, date(), 5),
          gc_agent:collect(),
          ?SLEEP,
          ?assertMatch([{identity, root}, {credit, _}, {cash, _}, {balance, _}, {today_turnover, 50}, {week_turnover, 50}], gc_agent:detail(root)),
          ?assertMatch([{identity, lv2a}, {credit, _}, {cash, _}, {balance, _}, {today_turnover, 10}, {week_turnover, 10}], gc_agent:detail(lv2a)),
          ?assertMatch([{identity, lv1a}, {credit, _}, {cash, _}, {balance, _}, {today_turnover, 25}, {week_turnover, 25}], gc_agent:detail(lv1a))
      end
    ]).

setup() ->
  Root = #tab_agent{aid = ?ROOT_ID, identity = ?ROOT_KEY, level = ?GC_ROOT_LEVEL, parent = root, cash = 0, credit = 0},
  Lv1a = #tab_agent{aid = ?LV1A_ID, identity = ?LV1A_KEY, level = ?GC_ROOT_LEVEL + 1, parent = root, cash = 10, credit = 10},
  Lv1b = #tab_agent{aid = ?LV1B_ID, identity = ?LV1B_KEY, level = ?GC_ROOT_LEVEL + 1, parent = root, cash = 10, credit = 10},
  Lv2a = #tab_agent{aid = ?LV2A_ID, identity = ?LV2A_KEY, level = ?GC_ROOT_LEVEL + 2, parent = ?LV1A_KEY, cash = 10, credit = 10},

  meck:new(gc_db),
  lists:map(fun({N, F}) -> meck:expect(gc_db, N, F) end, 
    [
      {get_turnover, fun (?LV1A_ID) -> [{?DATE, 5}];
                         (?LV1B_ID) -> [{?DATE, 5}];
                         (?LV2A_ID) -> [{?DATE, 5}];
                         (?ROOT_ID) -> [{?DATE, 10}] end},

      {get_collection_list, fun (?ROOT_KEY) -> [{?LV1A_KEY}, {?LV1B_KEY}];
                                (?LV1A_KEY) -> [{?LV2A_KEY}];
                                (?LV1B_KEY) -> [];
                                (?LV2A_KEY) -> [] end },
      {monitor, fun (_) -> ok end }
    ]),

  ?assertEqual(false, is_pid(whereis(gc_agent:to_pid(?ROOT_KEY)))),
  R = lists:map(fun (Agt) -> gc_agent:start_link(Agt), Agt#tab_agent.identity end,
    [Root, Lv1a, Lv1b, Lv2a]).

cleanup(R) ->
  lists:map(fun (Identity) -> gc_agent:stop(Identity) end, R),
  meck:unload(gc_db).
