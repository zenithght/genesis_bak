-module(game).
-behaviour(exch).

-export([id/0, init/2, stop/1, dispatch/2, call/2]).
-export([start/0, start/1, start/2]).

-include("common.hrl").
-include("schema.hrl").
-include("game.hrl").
-include("protocol.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%
%%% callback
%%% 

id() ->
  counter:bump(game).

init(GID, R = #tab_game_config{}) ->
  create_runtime(GID, R),
  #texas {
    gid = GID,
    limit = R#tab_game_config.limit,
    seats = seat:new(R#tab_game_config.seat_count)
  }.

stop(#texas{gid = GID, timer = Timer}) ->
  catch erlang:cancel_timer(Timer),
  clear_runtime(GID).

call(_, Ctx) ->
  {ok, ok, Ctx}.

dispatch({timeout, _, _Msg}, Ctx) ->
  Ctx;

dispatch(#join{}, Ctx) ->
  Ctx;

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

start(Conf) ->
  start(Conf, 1).

start(_Conf, 0) -> ok;
start(Conf = #tab_game_config{module = Module}, N) ->
  exch:start(Module, [Conf, texas_holdem_mods()]),
  start(Conf, N - 1).

%%%
%%% private
%%%

create_runtime(GID, R) ->
  ok = mnesia:dirty_write(#tab_game_xref {
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

core_texas_mods() ->
  [
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
    {showdown, []}
  ].

texas_holdem_mods() ->
  [ {wait_players, []} ] 
  ++ core_texas_mods() 
  ++ [ {restart, []} ].

%%%
%%% unit test
%%%

start_test() ->
  setup(),
  ok.

setup() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data().

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

