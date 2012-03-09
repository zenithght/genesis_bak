-module(sim_game_pre).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include("game.hrl").

-define(DEF_PLAYER, sim_client:player(jack)).
-define(DEF_PLAYER_ID, (sim_client:player(jack))#tab_player_info.identity).

-include_lib("eunit/include/eunit.hrl").

info_and_balance_query_test() ->
  run_by_login(fun() ->
        send(#player_query{}),
        send(#balance_query{}),
        ?assertMatch(#player_info{nick = <<"Jack">>, photo = <<"default">>}, head()),
        ?assertMatch(#balance{inplay = 0, amount = 0}, head())
    end).

list_test() ->
  run_by_login(fun() ->
        start_game(), % start two game
        send(#game_query{}),

        % match two game by head
        ?assertMatch(#game_info{
          table_name = <<"TEXAS_TABLE">>, 
          limit = #limit{min = 100, max = 400, small = 5, big = 10}, 
          seat_count = 9, required = 2, joined = 0
        }, head()),
        ?assertMatch(#game_info{
          table_name = <<"TEXAS_TABLE">>, 
          limit = #limit{min = 100, max = 400, small = 5, big = 10}, 
          seat_count = 9, required = 2, joined = 0
        }, head())
    end).

seat_query_test() ->
  run_by_login(fun() ->
        start_game(),
        send(#seat_query{game = 1}),
        SeatList = box(),
        ?assertEqual(9, length(SeatList)),
        lists:map(fun(R) ->
              ?assertMatch(#seat_state{state = ?PS_EMPTY, nick = <<"">>, photo = <<"">>}, R)
          end, SeatList)
    end
  ).

watch_test() ->
  run_by_login(fun() ->
        start_game(),
        send(#seat_query{game = 1}),
        ?assertMatch(T when length(T) =:= 9, box()),
        ?assertMatch(#texas{observers = []}, game:ctx(1)),
        send(#watch{game = 1}),
        ?assertMatch(#notify_game_detail{stage = ?GS_CANCEL, pot = 0, players = 0, seats = 9}, head()),
        ?assertMatch(#texas{observers = [{"jack", PID}]} when is_pid(PID), game:ctx(1))
    end
  ).

run_by_login(Fun) ->
  schema:init(),
  mnesia:dirty_write(?DEF_PLAYER),

  sim_client:kill_games(),
  sim_client:kill_player(?DEF_PLAYER_ID),

  sim_client:start(?MODULE),
  sim_client:send(?MODULE, #login{usr = list_to_binary(?DEF_PLAYER_ID), pass = <<?DEF_PWD>>}),

  ?assertMatch(#player_info{}, sim_client:head(?MODULE)),
  ?assertMatch(#balance{}, sim_client:head(?MODULE)),

  Fun().

start_game() ->
  Limit = #limit{min = 100, max = 400, small = 5, big = 10},
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}], limit = Limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2},
  game:start(Conf).
  
where() ->
  sim_client:where(?MODULE).

send(R) ->
  sim_client:send(?MODULE, R).

head() ->
  sim_client:head(?MODULE).

box() ->
  sim_client:box(?MODULE).
