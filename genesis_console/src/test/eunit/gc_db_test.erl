-module(gc_db_test).
-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

db_get_all_test_() -> {spawn, {setup, fun setup/0, fun cleanup/1, [
      fun () ->
          ?assertEqual([], gc_db:get_all(tab_turnover_log)),
          ?assertEqual(ok, mnesia:dirty_write(#tab_turnover_log{})),
          ?assertMatch([#tab_turnover_log{}], gc_db:get_all(tab_turnover_log))
      end
    ]}}.

db_get_collection_list_test_() -> {spawn, {setup, fun setup/0, fun cleanup/1, [
      fun () ->
          ?assertEqual([], gc_db:get_collection_list(root)),
          ?assertEqual(ok, mnesia:dirty_write(#tab_agent{parent = root, identity = new})),
          ?assertEqual([{new}], gc_db:get_collection_list(root))
      end
    ]}}.

setup() ->
  ?assertEqual(stopped, mnesia:stop()),
  ?assertEqual(ok, mnesia:delete_schema([node()])),
  ?assertEqual(ok, mnesia:create_schema([node()])),
  ?assertEqual(ok, mnesia:start()),
  ?assertEqual(ok, schema:rebuild_core()).

cleanup(_) ->
  ok.
