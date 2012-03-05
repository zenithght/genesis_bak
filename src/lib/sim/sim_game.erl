-module(sim_game).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
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

game_query_test() ->
  run_by_login(fun() ->
        ok
    end).

game_query2_test() ->
  run_by_login(fun() ->
        ok
    end).

run_by_login(Fun) ->
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
