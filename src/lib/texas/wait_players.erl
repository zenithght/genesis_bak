-module(wait_players).
-export([start/2, wait_for_players/2]).

-include("common.hrl").
-include("game.hrl").

start(_Params, Ctx = #texas{start_delay = StartDelay}) ->
  Timer = erlang:start_timer(StartDelay, self(), ?MODULE),
  {next, wait_for_players, Ctx#texas{ timer = Timer }}.

wait_for_players({timeout, _, ?MODULE}, Ctx = #texas{required = R, joined = J}) when J < R ->
  ?LOG([{texas, wait_for_players}, {not_required_players, J, R}]),
  {repeat, Ctx};

wait_for_players({timeout, _, ?MODULE}, Ctx = #texas{}) ->
  ?LOG([{texas, wait_for_players}, {start_playing, J, R}]),
  {stop, Ctx};

wait_for_players(_, Ctx) ->
  ?LOG([{texas, {wait_for_players, skip}}]),
  {skip, Ctx}.
