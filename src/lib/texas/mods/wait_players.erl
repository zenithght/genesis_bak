-module(wait_players).
-export([start/2, wait_for_players/2]).

-include("common.hrl").
-include("protocol.hrl").
-include("game.hrl").

start(_Params, Ctx = #texas{start_delay = StartDelay}) ->
  Timer = erlang:start_timer(StartDelay, self(), ?MODULE),
  {next, wait_for_players, Ctx#texas{ timer = Timer }}.

wait_for_players({timeout, _, ?MODULE}, Ctx = #texas{required = R, joined = J}) when J < R ->
  game:broadcast(#notify_game_cancel{game = Ctx#texas.gid}, Ctx),
  {repeat, Ctx};

wait_for_players({timeout, _, ?MODULE}, Ctx = #texas{seats = Seats}) ->
  ReadySeats = seat:lookup(?PS_READY, Seats),
  case length(ReadySeats) >= Ctx#texas.required of
    true ->
      game:broadcast(#notify_game_start{game = Ctx#texas.gid}, Ctx),
      ReadySeats = seat:lookup(?PS_READY, Seats),
      PlaySeats = seat:set(ReadySeats, ?PS_PLAY, Seats),
      {stop, Ctx#texas{seats = PlaySeats}};
    _ ->
      game:broadcast(#notify_game_cancel{game = Ctx#texas.gid}, Ctx),
      {repeat, Ctx}
  end;

wait_for_players(_R, Ctx) ->
  {skip, Ctx}.
