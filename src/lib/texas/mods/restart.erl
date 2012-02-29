-module(restart).
-export([start/2]).

-include("common.hrl").
-include("game.hrl").

start([], Ctx) ->
  ?LOG([{texas, restart}]),
  {goto, top, Ctx}.
