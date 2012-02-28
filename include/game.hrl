-define(PLAYER_TIMEOUT, 15000).
-define(PLAYER_OUT_TIMEOUT, 15000).
-define(START_DELAY, 6000).

%%% Tournaments

-define(TT_SIT_GO, 0). % starts when N players register
-define(TT_NORMAL, 1). % starts at a given time
-define(TT_REBUY, 2). 

%%% Game stage

-define(GS_CANCEL, 254).
-define(GS_PREFLOP, 0).
-define(GS_FLOP, 1).
-define(GS_TURN, 2).
-define(GS_RIVER, 3).
-define(GS_DELAYED_START, 4).
-define(GS_BLINDS, 5).
-define(GS_SHOWDOWN, 6).

%%% Game type

-define(GT_TEXAS_HOLDEM, 0).
-define(GT_IRC_TEXAS, 1). % IRC poker db

%%% Limit type

-define(LT_FIXED_LIMIT, 0).
-define(LT_NO_LIMIT, 1).
-define(LT_POT_LIMIT, 2).

-record(limit, {
    type,
    low,
    high,
    min,
    max
  }).

%%% Query operator

-define(OP_IGNORE, 0).
-define(OP_EQUAL, 1).
-define(OP_LESS, 2).
-define(OP_GREATER, 3).

-record(query_op, {
    op,
    val
  }).

%%% Player state

-define(PS_EMPTY, 0).
-define(PS_PLAY, 1).
-define(PS_FOLD, 2).
-define(PS_WAIT_BB, 4).
-define(PS_SIT_OUT, 8).
-define(PS_MAKEUP_BB, 16).
-define(PS_ALL_IN, 32).
-define(PS_BET, 64). 
-define(PS_RESERVED, 128). % reserved seat
-define(PS_AUTOPLAY, 256).
-define(PS_MUCK, 512). % will show cards
-define(PS_OUT, 1024). % can't play anymore

-define(PS_ANY, 
  ?PS_PLAY bor
  ?PS_FOLD bor
  ?PS_WAIT_BB bor
  ?PS_SIT_OUT bor
  ?PS_MAKEUP_BB bor
  ?PS_ALL_IN bor
  ?PS_BET bor 
  ?PS_AUTOPLAY).

-define(PS_ACTIVE, 
  ?PS_PLAY bor 
  ?PS_MAKEUP_BB).

-define(PS_BB_ACTIVE, 
  ?PS_PLAY bor
  ?PS_WAIT_BB bor
  ?PS_MAKEUP_BB).

-define(PS_READY,
  ?PS_STANDING bor
  ?PS_BB_ACTIVE bor
  ?PS_FOLD).

-define(PS_SHOWDOWN, 
  ?PS_PLAY bor
  ?PS_BET bor
  ?PS_ALL_IN).

-define(PS_STANDING, 
  ?PS_PLAY bor
  ?PS_ALL_IN bor
  ?PS_BET).

-define(PS_CAN_LEAVE,
  ?PS_FOLD bor
  ?PS_OUT bor
  ?PS_WAIT_BB bor
  ?PS_SIT_OUT bor
  ?PS_MAKEUP_BB).

%%% Face

-define(CF_ACE, 13).
-define(CF_KING, 12).
-define(CF_QUEEN, 11).
-define(CF_JACK, 10).
-define(CF_TEN, 9).
-define(CF_NINE, 8).
-define(CF_EIGHT, 7).
-define(CF_SEVEN, 6).
-define(CF_SIX, 5).
-define(CF_FIVE, 4).
-define(CF_FOUR, 3).
-define(CF_THREE, 2).
-define(CF_TWO, 1).
-define(CF_NONE, 0).

%%% Suit

-define(CS_SPADES, 4).
-define(CS_HEARTS, 3).
-define(CS_DIAMONDS, 2).
-define(CS_CLUBS, 1).
-define(CS_NONE, 0).

%%% Hand combination

-define(HC_HIGH_CARD, 0).
-define(HC_PAIR, 1).
-define(HC_TWO_PAIR, 2).
-define(HC_THREE_KIND, 3).
-define(HC_STRAIGHT, 4).
-define(HC_FLUSH, 5).
-define(HC_FULL_HOUSE, 6).
-define(HC_FOUR_KIND, 7).
-define(HC_STRAIGHT_FLUSH, 8).

-record(hand, {
    player = none,
    pid = none,
    cards = [], 
    rank = none,
    high1 = none,
    high2 = none,
    suit = ?CS_NONE,
    score = 0
  }).

-record(player_hand, {
    rank = ?HC_HIGH_CARD,
    high1 = ?CF_NONE,
    high2 = ?CF_NONE,
    suit = ?CS_NONE
  }).

-record(seat, {
    pid = ?UNDEF,               %% player id
    player = ?UNDEF,            %% player process
    hand = [],                  %% cards
    bet = 0,                    %% total bet
    inplay = 0,                 %% inplay balance
    state = ?PS_EMPTY           %% player state
  }).

-record(texas, {
    gid,                        %% runtime id
    seats,                      %% seat record list
    limit,                      %% limit record
    timeout = ?PLAYER_TIMEOUT,  %% player action timeout
    start_delay = ?START_DELAY, %% before start delay
    xref,     %% player to seat cross-reference
    pot,            %% pot structure
    deck,          %% card deck
    board = [],                 %% shared cards list
    observers = [],             %% game observers
    required = 2,               %% players required to start a game
    joined = 0,
    b = ?UNDEF,                 %% button
    sb = ?UNDEF,                %% small blind
    bb = ?UNDEF,                %% big blind
    timer = ?UNDEF,
    have_blinds = false, 
    headsup = false,
    sb_amt = 0,
    bb_amt = 0,
    raise_count = 0,            %% number of raises so far
    call = 0,
    exp_player = none,          %% expecting player
    exp_seat = none,            %% expecting seat
    exp_amt = 0,                %% expecting amount
    exp_min = 0,                %% expecting min amount
    exp_max = 0,                %% expecting max amount
    stage = ?GS_CANCEL,         %% current stage
    winners = []                %% last winners
  }).

