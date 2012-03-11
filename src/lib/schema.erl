-module(schema).
-export([init/0, install/0, uninstall/0, load_default_data/0]).

-include("schema.hrl").
-include("common.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(RAM, {ram_copies, Nodes}).
-define(DISC, {disc_copies, Nodes}).

-define(TABLE_DEF(Name, Type, Copies, Fields), {Name, [Copies, {type, Type}, {attributes, Fields}]}).

install() ->
  install([node()]).

install(Nodes) when is_list(Nodes) ->
  case mnesia:create_schema(Nodes) of
    {error, Reason} -> 
      {error, Reason};
    _ ->
      RamTables = [
        ?TABLE_DEF(tab_game_xref, set, ?RAM, record_info(fields, tab_game_xref)),
        ?TABLE_DEF(tab_player, set, ?RAM, record_info(fields, tab_player))
      ],
      DiscTables = [
        ?TABLE_DEF(tab_agent, set, ?DISC, record_info(fields, tab_agent)),
        ?TABLE_DEF(tab_player_info, set, ?DISC, record_info(fields, tab_player_info)),
        ?TABLE_DEF(tab_inplay, set, ?DISC, record_info(fields, tab_inplay)),
        ?TABLE_DEF(tab_game_config, set, ?DISC, record_info(fields, tab_game_config)),
        ?TABLE_DEF(tab_cluster_config, set, ?DISC, record_info(fields, tab_cluster_config)),
        ?TABLE_DEF(tab_counter, set, ?DISC, record_info(fields, tab_counter)),
        ?TABLE_DEF(tab_charge_log, bag, ?DISC, record_info(fields, tab_charge_log)),
        ?TABLE_DEF(tab_turnover_log, bag, ?DISC, record_info(fields, tab_turnover_log)),
        ?TABLE_DEF(tab_buyin_log, bag, ?DISC, record_info(fields, tab_buyin_log))
      ],

      mnesia:start(),

      create_tables(RamTables),
      create_tables(DiscTables),

      create_indices(tab_agent, [identity, parent]),
      create_indices(tab_player_info, [agent, identity]),
      create_indices(tab_buyin_log, [aid, pid, date]),
      create_indices(tab_turnover_log, [aid, pid, date]),
      create_indices(tab_charge_log, [aid, target, date])
  end.

uninstall() ->
  stopped = mnesia:stop(),
  ok = mnesia:delete_schema([node()]).

load_default_data() ->
  setup_counters(),
  setup_cluster([node()]),
  setup_agent(),
  setup_games().

init() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data().


%% Private

create_tables([]) -> ok;
create_tables([{Name, TabDef}|T]) ->
  {atomic, ok} = mnesia:create_table(Name, TabDef),
  create_tables(T).

create_indices(_, []) -> ok;
create_indices(Name, Index) when is_atom(Index) ->
  create_indices(Name, [Index]);
create_indices(Name, [Index|T]) ->
  {atomic, ok} = mnesia:add_table_index(Name, Index),
  create_indices(Name, T).

setup_cluster(Nodes) ->
  %% cluster configuration
  Conf = #tab_cluster_config {
    id = 0,
    mnesia_masters = Nodes,
    test_game_pass = <<"@!%#%2E35D$%#$^">>
  },
  F = fun() -> mnesia:write(Conf) end,
  {atomic, ok} = mnesia:transaction(F).

setup_counters()->
  counter:reset(game),
  counter:reset(player),
  counter:reset(inplay_xref),
  counter:reset(agent),
  ok.

setup_games() ->
  ok.

setup_agent() ->
  Root = #tab_agent{ aid = counter:bump(agent), identity = "root", password = erlang:md5("password"), root = true, parent = nil, cash = 100 * 10000 },
  {atomic, _} = mnesia:transaction( fun() -> mnesia:write(Root) end).
  
%% EUnit Test Case

uninstall_test() ->
  ?assertEqual(ok, uninstall()).

install_test() ->
  ?assertEqual(ok, uninstall()),
  ?assertEqual(ok, install()).

reinstall_raise_error_test() ->
  ?assertEqual(ok, uninstall()),
  ?assertEqual(ok, install()),
  ?assertMatch({error, _}, install()).

buyin_log_test() ->
  BuyInLog = #tab_buyin_log{
    aid = 1,
    gid = 1,
    amt = 10,
    cash = -10,
    credit = 100
  },

  Id = now(),
  mnesia:dirty_write(BuyInLog#tab_buyin_log{id = Id, pid = 1, date = date(), time = time()}),
  mnesia:dirty_write(BuyInLog#tab_buyin_log{id = Id, pid = 2, date = date(), time = time()}),

  ?assertMatch([#tab_buyin_log{pid = 1}], mnesia:dirty_index_read(tab_buyin_log, 1, pid)),
  ?assertMatch([#tab_buyin_log{aid = 1}, #tab_buyin_log{aid = 1}], mnesia:dirty_index_read(tab_buyin_log, 1, aid)),
  ?assertMatch([#tab_buyin_log{pid = 1}, #tab_buyin_log{pid = 2}], mnesia:dirty_read(tab_buyin_log, Id)).

tab_inplay_test() ->
  mnesia:dirty_write(#tab_inplay{pid = 1, inplay = 500}),
  mnesia:transaction(fun() ->
        [Inplay] = mnesia:read(tab_inplay, 1, write),
        mnesia:delete_object(Inplay)
    end),
  ?assertMatch([], mnesia:dirty_read(tab_inplay, 1)).
        
  
