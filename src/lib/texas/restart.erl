-module(restart).
-export([start/2]).

-include("texas.hrl").

start([], Ctx) ->
  ?LOG([{texas, restart}]),
  {goto, top, Ctx}.
