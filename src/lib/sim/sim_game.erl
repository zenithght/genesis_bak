-module(sim_game).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include("game.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(GAME, 1).
-define(GAME_CTX, game:ctx(1)).
-define(JACK, jack).
-define(JACK_ID, 1).
-define(TOMMY, tommy).
-define(TOMMY_ID, 2).

-define(DELAY, 500).
-define(SLEEP, timer:sleep(?DELAY)).

join_empty_game_test() ->
  run_by_login_two_players(fun() ->
        Jack = sim_client:where_player(?JACK_ID),
        Tommy = sim_client:where_player(?TOMMY_ID),

        sim_client:send(?TOMMY, #watch{game = ?GAME}),
        ?assertMatch(#notify_game_detail{}, sim_client:head(?TOMMY)),

        sim_client:send(?JACK, #watch{game = ?GAME}),
        ?assertMatch(#notify_game_detail{}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #join{game = ?GAME, sn = 1, buyin = 500}),
        ?assertMatch(#notify_join{}, sim_client:head(?JACK)),

        ?assertMatch(#notify_join{player = Jack}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #join{game = ?GAME, sn = 2, buyin = 500}),
        ?assertMatch(#notify_join{player = Tommy}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_join{player = Tommy}, sim_client:head(?JACK))
    end).

auto_compute_seat_sn_test() ->
  run_by_login_two_players(fun() ->
        Jack = sim_client:where_player(?JACK_ID),
        sim_client:send(?JACK, #join{game = ?GAME, sn = 0, buyin = 500}),
        ?assertMatch(#notify_game_detail{}, sim_client:head(?JACK)),
        ?assertMatch(#notify_join{player = Jack, sn = 1}, sim_client:head(?JACK)),
        Tommy = sim_client:where_player(?TOMMY_ID),
        sim_client:send(?TOMMY, #join{game = ?GAME, sn = 0, buyin = 500}),
        ?assertMatch(#notify_game_detail{}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_join{player = Tommy, sn = 2}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_join{player = Tommy, sn = 2}, sim_client:head(?JACK))
    end).

blind_game_test() ->
  run_by_login_two_players([{wait_players, []}, {blinds, []}, {stop, []}], fun() ->
        join_and_start_game(),

        lists:map(fun(ID) ->
              ?assertMatch(#notify_button{button = 1}, sim_client:head(ID)),
              ?assertMatch(#notify_sb{sb = 1}, sim_client:head(ID)),
              ?assertMatch(#notify_bb{bb = 2}, sim_client:head(ID))
          end, [?JACK, ?TOMMY]),

        Ctx = ?GAME_CTX, 
        Seats = Ctx#texas.seats,
        ?assertMatch(#texas{b = #seat{sn = 1}, sb = #seat{sn = 1}, bb = #seat{sn = 2}, headsup = true}, Ctx),
        ?assertMatch(#seat{sn = 1, bet = 5, inplay = 495}, seat:get(1, Seats)),
        ?assertMatch(#seat{sn = 2, bet = 10, inplay = 490}, seat:get(2, Seats))
    end).
        
run_by_login_two_players(Fun) ->
  run_by_login_two_players([{wait_players, []}, {restart, []}], Fun).

run_by_login_two_players(Mods, Fun) ->
  schema:init(),
  mnesia:dirty_write(sim_client:player(?JACK)),
  mnesia:dirty_write(sim_client:player(?TOMMY)),

  sim_client:kill_games(),

  %% login Jack & Tommy
  lists:map(fun({Key, Id}) ->
        Usr = list_to_binary((sim_client:player(Key))#tab_player_info.identity),
        sim_client:kill_player(Id),
        sim_client:start(Key),
        sim_client:send(Key, #login{usr = Usr, pass = <<?DEF_PWD>>}),
        ?assertMatch(#player_info{}, sim_client:head(Key)),
        ?assertMatch(#balance{}, sim_client:head(Key))
    end, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]),

  Limit = #limit{min = 100, max = 400, small = 5, big = 10},
  Conf = #tab_game_config{module = game, mods = Mods, limit = Limit, seat_count = 9, start_delay = ?DELAY, required = 2, timeout = 1000, max = 1},
    
  game:start(Conf),
  Fun().

join_and_start_game() ->
  Jack = sim_client:where_player(?JACK_ID),
  Tommy = sim_client:where_player(?TOMMY_ID),
  sim_client:send(?JACK, #join{game = ?GAME, sn = 1, buyin = 500}),
  sim_client:send(?TOMMY, #join{game = ?GAME, sn = 2, buyin = 500}),
  ?assertMatch(#notify_game_detail{}, sim_client:head(?JACK)),
  ?assertMatch(#notify_game_detail{}, sim_client:head(?TOMMY)),
  ?assertMatch(#notify_join{player = Jack}, sim_client:head(?JACK)),
  ?assertMatch(#notify_join{player = Tommy}, sim_client:head(?JACK)),
  ?assertMatch(#notify_join{player = Tommy}, sim_client:head(?TOMMY)),
  ?SLEEP,
  ?assertMatch(#notify_start_game{}, sim_client:head(?JACK)),
  ?assertMatch(#notify_start_game{}, sim_client:head(?TOMMY)),
  ?SLEEP.
