-module(gc_monitor_test).
-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

monitor_test_() -> ?SPAWN_TEST([
      fun () ->
          ?assertEqual(ok, mnesia:dirty_write(#tab_agent_player{pid = 1, aid = root})),
          ?assertEqual(ok, mnesia:dirty_write(#tab_agent_player{pid = 2, aid = root})),

          ?SLEEP,

          ?assertEqual(ok, mnesia:dirty_write(#tab_turnover_log{pid = 1, date = date(), amt = 10})),
          ?assertEqual(ok, mnesia:dirty_write(#tab_turnover_log{pid = 2, date = date(), amt = 10})),
          ?SLEEP,

          ?assertEqual(2, gc_monitor:log_counter())
      end
    ]).

setup() ->
  ?assertEqual(stopped, mnesia:stop()),
  ?assertEqual(ok, mnesia:delete_schema([node()])),
  ?assertEqual(ok, mnesia:create_schema([node()])),
  ?assertEqual(ok, mnesia:start()),
  ?assertEqual(ok, schema:rebuild_core()),

  ?assertEqual(false, is_pid(whereis(gc_monitor))),
  gc_monitor:start_link().

cleanup(_) ->
  gc_monitor:stop().
