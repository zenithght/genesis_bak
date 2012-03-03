-module(sim_test).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SLEEP, timer:sleep(500)).
-define(SLEEP(T), timer:sleep(T * 1000)).

%% test dirty code, redefine client module pdata record.
-record(pdata, { 
    timer = ?UNDEF, 
    server = global:whereis_name(server),
    player = ?UNDEF 
  }).

%% TODO test timeout is use long time, commit befor complate all test
connection_timeout() ->
  run_by_def(fun(_) ->
        ?assertEqual(true, is_pid(where())),
        ?SLEEP(3),
        [E] = sim_client:box(),
        ?assertEqual(undefined, where()),
        ?assertEqual(E#bad.error, ?ERR_CONNECTION_TIMEOUT)
    end).

login_unauth_test() ->
  run_by_def(fun([{player, I}|_]) ->
        mnesia:dirty_write(I#tab_player_info{identity = "player_new"}),
        ?assertEqual(true, is_pid(where())),
        send(#login{usr = <<"player">>, pass = <<"def_pwd">>}),
        [E] = sim_client:box(),
        ?assertEqual(undefined, where()),
        ?assertEqual(E#bad.error, ?ERR_UNAUTH)
    end),
  run_by_def(fun([{player, I}|_]) ->
        mnesia:dirty_write(I#tab_player_info{password = "pwd"}),
        ?assertEqual(true, is_pid(where())),
        send(#login{usr = <<"player">>, pass = <<"def_pwd">>}),
        [E] = sim_client:box(),
        ?assertEqual(undefined, where()),
        ?assertEqual(E#bad.error, ?ERR_UNAUTH)
    end).

login_player_disbale_test() ->
  run_by_def(fun([{player, I}|_]) ->
        mnesia:dirty_write(I#tab_player_info{disabled = true}),
        ?assertEqual(true, is_pid(where())),
        send(#login{usr = <<"player">>, pass = <<"def_pwd">>}),
        [E] = sim_client:box(),
        ?assertEqual(undefined, where()),
        ?assertEqual(E#bad.error, ?ERR_PLAYER_DISABLE)
    end).

login_successful_test() ->
  run_by_def(fun([{player, I}|_]) ->
        mnesia:dirty_write(I),
        ?assertEqual(true, is_pid(where())),
        send(#login{usr = <<"player">>, pass = <<"def_pwd">>}),
        Data = sim_client:loopdata(),
        ?assertEqual(true, is_pid(Data#pdata.player))
    end).

%%%
%%% private
%%%

-define(DEF_PWD, "def_pwd").
-define(DEF_HASH_PWD, erlang:phash2(?DEF_PWD, 1 bsl 32)).

run_by_def(Fun) ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data(),

  Data = [
    { player, #tab_player_info{
      pid = 1, 
      identity = "player", 
      password = ?DEF_HASH_PWD,
      disabled = false }}],

  sim_client:flush(),
  sim_client:kill(player),
  sim_client:start(player),

  Fun(Data).

where() ->
  sim_client:where(player).

send(R) ->
  sim_client:send(player, R).
