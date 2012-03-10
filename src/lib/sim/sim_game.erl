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

start_game_test() ->
  run_by_login_two_players(fun() ->
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
        ?assertMatch(#notify_start_game{}, sim_client:head(?TOMMY))
    end).

run_by_login_two_players(Fun) ->
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
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}, {restart, []}], limit = Limit, seat_count = 9, start_delay = ?DELAY, required = 2, timeout = 1000, max = 1},
    
  game:start(Conf),
  Fun().
