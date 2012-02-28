-module(game).
-behaviour(exch).

-export([id/0, init/2, stop/1, dispatch/2, call/2]).
-export([start/0, start/1, start/2]).
-export([join/1]).

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
    xref = gb_trees:empty(),
    pot = pot:new(),
    deck = deck:new()
  }.

stop(#texas{gid = GID, timer = Timer}) ->
  catch erlang:cancel_timer(Timer),
  clear_runtime(GID).

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
start(Conf) when is_record(Conf, tab_game_config)->
  start(Conf, 1).

start(_Conf, 0) -> ok;
start(Conf = #tab_game_config{module = Module, mods = Mods}, N) when is_list(Mods) ->
  exch:start(Module, Conf, Mods),
  start(Conf, N - 1);
start(Conf = #tab_game_config{}, N) ->
  start(Conf#tab_game_config{mods = default_mods()}, N).

join(Id) ->
  gen_server:cast(?LOOKUP_GAME(Id), join).
  

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
    {rank, []}, 
    %% start after BB, 3 raises
    {betting, [?MAX_RAISES, ?GS_PREFLOP, true]}, 
    %% show 3 shared cards
    {deal_cards, [3, shared]}, 
    {rank, []}, 
    %% flop
    {betting, [?MAX_RAISES, ?GS_FLOP]}, 
    %% show 1 more shared card
    {deal_cards, [1, shared]}, 
    {rank, []}, 
    %% turn
    {betting, [?MAX_RAISES, ?GS_TURN]}, 
    %% show 1 more shared card
    {deal_cards, [1, shared]}, 
    {rank, []}, 
    %% river
    {betting, [?MAX_RAISES, ?GS_RIVER]}, 
    %% showdown
    {showdown, []},
    {restart, []}
  ].


%%% TODO search game

%send_games(_, [], _) ->
  %ok;

%send_games(Socket, [H|T], C) ->
  %N = H#game_info{game_count = C},
  %?tcpsend(Socket, N),
  %send_games(Socket, T, C).

%find_games(Socket, 
           %GameType, LimitType,
           %#query_op{ op = ExpOp, val = Expected }, 
           %#query_op{ op = JoinOp, val = Joined },
           %#query_op{ op = WaitOp, val = Waiting }) ->
    %{atomic, L} = g:find(GameType, LimitType,
                         %ExpOp, Expected, 
                         %JoinOp, Joined,
                         %WaitOp, Waiting),
    
    %send_games(Socket, L, lists:flatlength(L)).

  %%% TODO kill games
%kill_games() ->
    %{atomic, Games} = db:find(tab_game_xref),
    %kill_games(Games).

%kill_games([]) ->
    %ok;

%kill_games([H|T]) ->
    %gen_server:cast(H#tab_game_xref.process, stop),
    %kill_games(T).

%%%
%%% unit test
%%%

-include_lib("eunit/include/eunit.hrl").

start_test() ->
  setup(),
  game:start([{wait_players, []}, {restart, []}]),
  ?assert(is_pid(?LOOKUP_GAME(1))),
  timer:sleep(2000),
  game:join(1),
  timer:sleep(500),
  game:join(1),
  timer:sleep(5000).

setup() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data().
