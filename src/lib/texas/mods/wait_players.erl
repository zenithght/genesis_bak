-module(wait_players).
-export([start/2, wait_for_players/2]).

-include("common.hrl").
-include("protocol.hrl").
-include("game.hrl").

start(_Params, Ctx = #texas{start_delay = StartDelay}) ->
  Timer = erlang:start_timer(StartDelay, self(), ?MODULE),
  {next, wait_for_players, Ctx#texas{ timer = Timer }}.

wait_for_players({timeout, _, ?MODULE}, Ctx = #texas{required = R, joined = J}) when J < R ->
  game:broadcast(#notify_cancel_game{game = Ctx#texas.gid}, Ctx),
  {repeat, Ctx};

wait_for_players({timeout, _, ?MODULE}, Ctx = #texas{seats = Seats}) ->
  game:broadcast(#notify_start_game{game = Ctx#texas.gid}, Ctx),
  ReadySeats = seat:lookup(?PS_READY, Seats),
  PlaySeats = seat:set(ReadySeats, ?PS_PLAY, Seats),
  {stop, Ctx#texas{seats = PlaySeats}};

wait_for_players(_R, Ctx) ->
  {skip, Ctx}.
