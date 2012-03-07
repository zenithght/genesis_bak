-module(game).
-behaviour(exch).

-export([id/0, init/2, stop/1, dispatch/2, call/2]).
-export([start/0, start/1, start/2]).
-export([join/1, bet/2, reward/3, broadcast/2, broadcast/3, info/1, list/0]).

-include("common.hrl").
-include("schema.hrl").
-include("game.hrl").
-include("protocol.hrl").

-define(LOOKUP_GAME(Id), global:whereis_name({game, Id})).

%%%
%%% callback
%%% 

id() ->
  counter:bump(game).

init(GID, R = #tab_game_config{}) ->
  create_runtime(GID, R),
  #texas { 
    gid = GID, 
    seats = seat:new(R#tab_game_config.seat_count),
    limit = R#tab_game_config.limit,
    timeout = ?PLAYER_TIMEOUT,
    start_delay = R#tab_game_config.start_delay,
    required = R#tab_game_config.required,
    xref = gb_trees:empty(),
    pot = pot:new(),
    deck = deck:new()
  }.

stop(#texas{gid = GID, timer = Timer}) ->
  catch erlang:cancel_timer(Timer),
  clear_runtime(GID).

call(info, Ctx = #texas{gid = GId, joined = Joined, required = Required, seats = Seats, limit = Limit}) ->
  {ok, #game_info{
      game = GId,
      table_name = "TEXAS_TABLE",
      type = "TEXAS",
      limit = Limit,
      seat_count = seat:info(size, Seats),
      required = Required,
      joined = Joined
    }, Ctx};

call(_, Ctx) ->
  {ok, ok, Ctx}.

dispatch(join, Ctx = #texas{joined = Joined}) ->
  Ctx#texas{joined = Joined + 1};

dispatch(#leave{}, Ctx) ->
  Ctx;

dispatch(#watch{}, Ctx) ->
  Ctx;

dispatch(#unwatch{}, Ctx) ->
  Ctx;

dispatch(_, Ctx) ->
  Ctx.

%%%
%%% client
%%%

start() ->
  Fun = fun(R = #tab_game_config{max = Max}, _Acc) ->
      start(R, Max)
  end, 

  ok = mnesia:wait_for_tables([tab_game_config], ?WAIT_TABLE),
  {atomic, _Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, nil, tab_game_config) end).

start(Mods) when is_list(Mods)->
  Conf = #tab_game_config{id = 1, module = game, mods = Mods, limit = no_limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 1},
  start(Conf, 1);

start(Conf = #tab_game_config{max = Max}) ->
  start(Conf, Max).

start(_Conf, 0) -> ok;
start(Conf = #tab_game_config{module = Module, mods = Mods}, N) when is_list(Mods) ->
  exch:start(Module, Conf, Mods),
  start(Conf, N - 1);
start(Conf = #tab_game_config{}, N) ->
  start(Conf#tab_game_config{mods = default_mods()}, N).

join(Id) ->
  gen_server:cast(?LOOKUP_GAME(Id), join).

bet({S = #seat{inplay = Inplay, bet = Bet, pid = PID}, Amt}, Ctx = #texas{pot = Pot, seats = Seats}) ->
  {State, AllIn} = case Amt < Inplay of 
    true -> {?PS_BET, false}; 
    _ -> {?PS_ALL_IN, true} 
  end,
  NewInplay = mnesia:dirty_update_counter(tab_inplay, PID, 0 - Amt),
  NewSeats = seat:set(S#seat{inplay = NewInplay, state = State, bet = Bet + Amt}, Seats),
  NewPot = pot:add(Pot, PID, Amt, AllIn),
  Ctx#texas{seats = NewSeats, pot = NewPot}.

reward(#hand{seat_sn = SN, pid = PId}, Amt, Ctx = #texas{seats = S}) when Amt > 0 ->
  NewInplay = mnesia:dirty_update_counter(tab_inplay, PId, Amt),
  Seat = seat:get(SN, S),
  RewardedSeats = seat:set(Seat#seat{inplay = NewInplay}, S),
  Ctx#texas{seats = RewardedSeats}.

broadcast(Msg, #texas{observers = Obs}, []) ->
  broadcast(Msg, Obs);
broadcast(Msg, Ctx = #texas{observers = Obs}, _Exclude = [H|T]) ->
  broadcast(Msg, Ctx#texas{observers = proplists:delete(H, Obs)}, T).

broadcast(_Msg, []) -> ok;
broadcast(Msg, #texas{observers = Obs}) -> 
  broadcast(Msg, Obs);
broadcast(Msg, [{_, Process}|T]) ->
  player:notify(Process, Msg),
  broadcast(Msg, T).

info(Game) when is_pid(Game) ->
  gen_server:call(Game, info).

list() ->
  Fun = fun(#tab_game_xref{process = Game}, Acc) ->
      [info(Game) | Acc]
  end,
  {atomic, Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], tab_game_xref) end),
  Result.

%%%
%%% private
%%%

create_runtime(GID, R) ->
  mnesia:dirty_write(#tab_game_xref {
      gid = GID,
      process = self(),
      module = R#tab_game_config.module,
      limit = R#tab_game_config.limit,
      seat_count = R#tab_game_config.seat_count,
      timeout = R#tab_game_config.timeout,
      required = R#tab_game_config.required
  }).

clear_runtime(GID) ->
  ok = mnesia:dirty_delete(tab_game_xref, GID).

default_mods() ->
  [
    {wait_players, []},
    %% blind rules
    {blinds, []},
    %% deal 2 cards to each player
    {deal_cards, [2, private]}, 
    {ranking, []}, 
    %% start after BB, 3 raises
    {betting, [?GS_PREFLOP]}, 
    %% show 3 shared cards
    {deal_cards, [3, shared]}, 
    {ranking, []}, 
    %% flop
    {betting, [?GS_FLOP]}, 
    %% show 1 more shared card
    {deal_cards, [1, shared]}, 
    {ranking, []}, 
    %% turn
    {betting, [?GS_TURN]}, 
    %% show 1 more shared card
    {deal_cards, [1, shared]}, 
    {ranking, []}, 
    %% river
    {betting, [?GS_RIVER]}, 
    %% showdown
    {showdown, []},
    {restart, []}
  ].

%%%
%%% unit test
%%%

-include_lib("eunit/include/eunit.hrl").

start_test() ->
  setup(),
  game:start([{wait_players, []}, {restart, []}]),
  ?assert(is_pid(?LOOKUP_GAME(1))).

id_test() ->
  setup(),
  ?assertEqual(1, game:id()),
  ?assertEqual(2, game:id()),
  ?assertEqual(3, game:id()).

list_test() ->
  setup(),
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}], limit = no_limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2},
  game:start(Conf),
  ?assert(is_pid(?LOOKUP_GAME(1))),
  ?assert(is_pid(?LOOKUP_GAME(2))),
  timer:sleep(1000),

  [#game_info{}|[#game_info{}|[]]] = game:list().

info_test() ->
  setup(),
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}], limit = no_limit, seat_count = 5, start_delay = 3000, required = 3, timeout = 1000, max = 1},
  game:start(Conf),
  ?assert(is_pid(?LOOKUP_GAME(1))),
  #game_info{required = R, seat_count = C} = game:info(?LOOKUP_GAME(1)),
  ?assertEqual(3, R),
  ?assertEqual(5, C).

setup() ->
  catch mnesia:transaction(fun() ->
        mnesia:foldl(fun(#tab_game_xref{process = Game}, _Acc) ->
              gen_server:call(Game, kill)
          end, [], tab_game_xref)
    end
  ),
  schema:uninstall(),
  schema:install(),
  schema:load_default_data().
