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
-define(FOO, foo).
-define(FOO_ID, 3).

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

-define(DELAY, 500).
-define(SLEEP, timer:sleep(?DELAY)).

betting_test() ->
  run_by_login_two_players([{blinds, []}, {betting, [?GS_PREFLOP]}], fun() ->
        join_and_start_game(?TWO_PLAYERS),
        SB = 1, BB = 2,
        check_blind(?TWO_PLAYERS, SB, SB, BB),

        %% sb = 10, call = 10, raise_max = 80
        ?assertMatch(#notify_actor{sn = SB}, sim_client:head(?JACK)),
        ?assertMatch(#notify_betting{call = 10, min = 20, max = 80}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}), %% call
        ?assertMatch(#notify_raise{game = ?GAME, raise = 0, call = 10}, sim_client:head(?JACK)),
        ?assertMatch(#notify_actor{sn = BB}, sim_client:head(?JACK)), %% turnover player

        ?assertMatch(#notify_actor{sn = SB}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_raise{game = ?GAME, raise = 0, call = 10}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_actor{sn = BB}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 80}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}) %% check
    end).

run_by_login_two_players(Fun) ->
  run_by_login_players([], ?TWO_PLAYERS, Fun).

run_by_login_two_players(Mods, Fun) ->
  run_by_login_players(Mods, ?TWO_PLAYERS, Fun).

run_by_login_players(MixinMods, Players, Fun) ->
  schema:init(),
  mnesia:dirty_write(sim_client:player(?TOMMY)),

  sim_client:kill_games(),

  %% login Jack & Tommy
  lists:map(fun({Key, Id}) ->
        mnesia:dirty_write(sim_client:player(Key)),
        Usr = list_to_binary((sim_client:player(Key))#tab_player_info.identity),
        sim_client:kill_player(Id),
        sim_client:start(Key),
        sim_client:send(Key, #cmd_login{identity = Usr, password = <<?DEF_PWD>>}),
        ?assertMatch(#notify_player{}, sim_client:head(Key)),
        ?assertMatch(#notify_acount{}, sim_client:head(Key))
    end, Players),
  Mods = [{wait_players, []}] ++ MixinMods ++ [{stop, []}],
  Limit = #limit{min = 100, max = 400, small = 10, big = 20},
  Conf = #tab_game_config{module = game, mods = Mods, limit = Limit, seat_count = 9, start_delay = ?DELAY, required = 2, timeout = 1000, max = 1},
    
  game:start(Conf),
  Fun().

join_and_start_game(Players) ->
  ok = join_and_start_game(Players, 1),
  ?SLEEP,
  Len = length(Players) - 1,
  [H|_] = lists:reverse(Players),
  check_notify_join(lists:delete(H, Players), Len, Len),
  check_notify_start(Players).

join_and_start_game([], _SN) -> ok;
join_and_start_game([{Key, Id}|T], SN) ->
  sim_client:send(Key, #cmd_join{game = ?GAME, sn = SN, buyin = 100}),
  ?assertMatch(#notify_game_detail{}, sim_client:head(Key)),
  ?assertMatch(#notify_join{player = Id}, sim_client:head(Key)),
  join_and_start_game(T, SN + 1).

check_blind([], _, _, _) -> ok;
check_blind([{Key, _Id}|T], B, SB, BB) ->
  ?assertMatch(#notify_button{b = B}, sim_client:head(Key)),
  ?assertMatch(#notify_sb{sb = SB}, sim_client:head(Key)),
  ?assertMatch(#notify_bb{bb = BB}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 10}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 20}, sim_client:head(Key)),
  check_blind(T, B, SB, BB).

check_notify_start([]) -> ok;
check_notify_start([{Key, _Id}|T]) ->
  ?assertMatch(#notify_game_start{}, sim_client:head(Key)),
  check_notify_start(T).

check_notify_join([], 0, 0) -> ok;
check_notify_join([_|T], 0, S) ->
  check_notify_join(T, S - 1, S - 1);
check_notify_join(Players = [{Key, _Id}|_], N, S) ->
  ?assertMatch(#notify_join{}, sim_client:head(Key)),
  check_notify_join(Players, N - 1, S).
