-module(sim_game).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include("game.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(JACK, jack).
-define(JACK_ID, "jack").
-define(TOMMY, tommy).
-define(TOMMY_ID, "tommy").

join_empty_game_test() ->
  run_by_login_two_players(fun() ->
        ok
    end).

run_by_login_two_players(Fun) ->
  schema:init(),
  mnesia:dirty_write(sim_client:player(?JACK)),
  mnesia:dirty_write(sim_client:player(?TOMMY)),

  sim_client:kill_games(),

  %% login Jack & Tommy
  lists:map(fun({Key, Id}) ->
        sim_client:kill_player(Id),
        sim_client:start(Key),
        sim_client:send(Key, #login{usr = list_to_binary(Id), pass = <<?DEF_PWD>>}),
        ?assertMatch(#player_info{}, sim_client:head(Key)),
        ?assertMatch(#balance{}, sim_client:head(Key))
    end, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]),

  Fun().

start_game() ->
  Limit = #limit{min = 100, max = 400, small = 5, big = 10},
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}], limit = Limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2},
  game:start(Conf).
