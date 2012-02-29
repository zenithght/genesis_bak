-module(limit).
-export([behaviour_info/1]).
-export([blinds/1]).

-include("game.hrl").

behaviour_info(callbacks) ->
  [{raise, 5}, {blinds, 2}].

blinds(#limit{type = Type, low = Low, high = High}) ->
  Type:blinds(Low, High).

