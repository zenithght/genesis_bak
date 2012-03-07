-module(sim_game).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include("game.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SLEEP, timer:sleep(500)).
-define(SLEEP(T), timer:sleep(T * 1000)).

-define(DEF_PWD, "def_pwd").
-define(DEF_HASH_PWD, erlang:phash2(?DEF_PWD, 1 bsl 32)).


info_and_balance_query_test() ->
  run_by_login(fun() ->
        send(#player_query{}),
        send(#balance_query{}),
        ?SLEEP,
        #player_info{nick = Nick, photo = Photo} = head(),
        ?assertEqual(<<"player">>, Nick),
        ?assertEqual(<<"default">>, Photo),
        #balance{inplay = Inplay, amount = Balance} = head(),
        ?assertEqual(0, Inplay),
        ?assertEqual(0, Balance)
    end).

list_test() ->
  run_by_login(fun() ->
        start_game(),
        send(#game_query{}),
        ?SLEEP,
        #game_info{} = head(),
        #game_info{} = head()
    end).

game_info_test() ->
  run_by_login(fun() ->
        ok
    end).

start_game() ->
  Limit = #limit{min = 100, max = 400, small = 5, big = 10},
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}], limit = Limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2},
  game:start(Conf).
  
run_by_login(Fun) ->
  catch mnesia:transaction(fun() ->
        mnesia:foldl(fun(#tab_game_xref{process = Game}, _Acc) ->
              gen_server:call(Game, kill)
          end, [], tab_game_xref)
    end
  ),
  schema:uninstall(),
  schema:install(),
  schema:load_default_data(),

  mnesia:dirty_write(#tab_player_info{
      pid = 1, 
      identity = "player", 
      nick = "player",
      photo = "default",
      password = ?DEF_HASH_PWD,
      disabled = false }),

  player:stop({kill, "player"}),

  sim_client:flush(),
  sim_client:kill(player),
  sim_client:start(player),

  %% sim player login and flush message
  send(#login{usr = <<"player">>, pass = <<"def_pwd">>}),
  ?SLEEP,
  #player_info{} = head(),
  #balance{} = head(),

  Fun(),
  sim_client:kill(player).

where() ->
  sim_client:where(player).

send(R) ->
  sim_client:send(player, R).

head() ->
  sim_client:head(player).
