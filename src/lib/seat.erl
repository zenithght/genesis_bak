-module(seat).
-export([new/1]).

-include("common.hrl").
-include("game.hrl").

new(N) when N > 0 ->
  erlang:make_tuple(N, #seat{}).
