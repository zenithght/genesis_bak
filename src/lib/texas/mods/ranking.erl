-module(ranking).
-export([start/2]).
-export([rank/1, notify/1]).

-include("game.hrl").
-include("protocol.hrl").

%%%
%%% callback
%%%

start([], Ctx) ->
  notify(Ctx),
  {stop, Ctx}.

%%%
%%% client
%%%

rank(#texas{seats = S, board = Cards}) ->
  Seats = seat:lookup(?PS_STANDING, S),
  rank(Seats, Cards, []).

notify(Ctx) ->
  notify(rank(Ctx), Ctx).

%%% 
%%% private
%%% 

rank([], _Cards, Acc) -> Acc;
rank([H = #seat{hand = Hand}|T], Cards, Acc) ->
  RH = hand:rank(hand:merge(Hand, Cards)),
  rank(T, Cards, [H#seat{hand = RH}|Acc]).

notify([], _Ctx) -> ok;
notify([#seat{pid = PId, process = P, hand = Hand}|T], Ctx = #texas{gid = Id}) ->
  PH = hand:player_hand(Hand),
  player:notify(P, #notify_hand{ player = PId, game = Id, hand = PH }),
  notify(T, Ctx).