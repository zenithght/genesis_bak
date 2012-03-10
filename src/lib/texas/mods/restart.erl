-module(restart).
-export([start/2]).

-include("common.hrl").
-include("protocol.hrl").
-include("game.hrl").

start([], Ctx) ->
  game:broadcast(#notify_end_game{game = Ctx#texas.gid}, Ctx),
  {goto, top, Ctx}.
