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
        ?TABLE_DEF(tab_balance, set, ?DISC, record_info(fields, tab_balance)),
        ?TABLE_DEF(tab_inplay, set, ?DISC, record_info(fields, tab_inplay)),
        ?TABLE_DEF(tab_game_config, set, ?DISC, record_info(fields, tab_game_config)),
        ?TABLE_DEF(tab_cluster_config, set, ?DISC, record_info(fields, tab_cluster_config)),
        ?TABLE_DEF(tab_counter, set, ?DISC, record_info(fields, tab_counter)),
        ?TABLE_DEF(tab_turnover, bag, ?DISC, record_info(fields, tab_turnover))
      ],

      mnesia:start(),

      create_tables(RamTables),
      create_tables(DiscTables),

      create_indices(tab_agent, [identity, parent]),
      create_indices(tab_turnover, [date]),
      create_indices(tab_player_info, [agent, identity])
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
  ?assert(ok == uninstall()).

install_test() ->
  ok = uninstall(),
  ok = install().

reinstall_raise_error_test() ->
  ok = uninstall(),
  ok = install(),
  {error, _} = install().
