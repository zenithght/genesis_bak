-module(blinds).
-export([start/2]).

-include("common.hrl").
-include("schema.hrl").
-include("game.hrl").
-include("protocol.hrl").

%%%
%%% callback
%%%

start([], Ctx = #texas{gid = Id, limit = Limit}) ->
  {SmallAmt, BigAmt} = limit:blinds(Limit),

  Button = advance_button(Ctx),
  game:notify(#notify_button{ game = Id, button = Button#seat.sn }, Ctx),

  {Small, Big, Headsup} = advance_blinds(Button, Ctx),
  game:notify(#notify_sb{ game = Id, sb = Small#seat.sn }, Ctx),
  game:notify(#notify_bb{ game = Id, bb = Big#seat.sn }, Ctx),

  BlindedCtx = blind([{Small, SmallAmt}, {Big, BigAmt}], Ctx),
  ?LOG([{exch, blind}, {button, Button}, {small, Small}, {big, Big}]),

  {stop, BlindedCtx#texas{
      sb_amt = SmallAmt, bb_amt = BigAmt,
      b = Button, sb = Small, bb = Big, headsup = Headsup
  }}.

%%
%% private
%%

advance_button(#texas{b = B, seats = S}) when B =:= ?UNDEF ->
  [H|_] = seat:lookup(?PS_PLAY, S), H;
advance_button(#texas{b = B, seats = S}) ->
  [H|_] = seat:lookup(?PS_PLAY, B, S), H.

advance_blinds(B, #texas{seats = S}) ->
  case seat:lookup(?PS_PLAY, B, S) of
    [Hb] -> % headsup game whit two player (button is small)
      {B, Hb, true};
    [Hs|[Hb|_]] ->
      {Hs, Hb, false}
  end.

blind([], Ctx) -> Ctx;
blind([{Seat = #seat{pid = PId, sn = SN}, Amt}|T], Ctx = #texas{gid = Id}) ->
  NewCtx = game:cost(Seat, Amt, Ctx),
  game:notify(#notify_blind{ game = Id, player = PId, call = Amt }, NewCtx),
  game:notify_state(SN, NewCtx),
  blind(T, NewCtx).
