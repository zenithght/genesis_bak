-module(sim_game).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include("game.hrl").

-include_lib("eunit/include/eunit.hrl").

join_empty_game_test() ->
  ok.
