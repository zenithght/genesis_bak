-ifndef(UNDEF).
  -define(UNDEF, undefined).
-endif.

-define(PLAYER_TIMEOUT, 15000).
-define(PLAYER_OUT_TIMEOUT, 15000).
-define(START_DELAY, 6000).

%%% Tournaments

-define(TT_SIT_GO, 0). % starts when N players register
-define(TT_NORMAL, 1). % starts at a given time
-define(TT_REBUY, 2). 

%%% Game stage

-define(GS_PREFLOP, 0).
-define(GS_FLOP, 1).
-define(GS_TURN, 2).
-define(GS_RIVER, 3).

%%% Game type

-define(GT_TEXAS_HOLDEM, 0).
-define(GT_IRC_TEXAS, 1). % IRC poker db

%%% Limit type

-define(LT_FIXED_LIMIT, 0).
-define(LT_NO_LIMIT, 1).
-define(LT_POT_LIMIT, 2).

-record(limit, {
    type,        % no_limit, pot_limit, fixed_limit
    small,       % small blind
    big,         % big blind
    low,         % betting low limit
    high,        % betting high limit
    min,         % min buyin
    max          % max buyin
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
-define(PS_PLAY, 1).      %% player is not bet or other player is raised
-define(PS_BET, 2).       %% player was betted
-define(PS_ALL_IN, 4).    %% player inplay all in pot
-define(PS_FOLD, 8).      %% player fold
-define(PS_OUT, 16).      %% player out when inplay not enough
-define(PS_LEAVE, 32).    %% player is leave but game is not over

-define(PS_STANDING,      %% player is survive game
  ?PS_PLAY bor
  ?PS_BET bor
  ?PS_ALL_IN).

-define(PS_READY,         %% player ready to play game again
  ?PS_STANDING bor
  ?PS_FOLD).

-define(PS_ANY, 
  ?PS_STANDING bor
  ?PS_FOLD bor
  ?PS_LEAVE).

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
    pid = ?UNDEF,
    seat_sn = ?UNDEF,
    cards = [], 
    rank = ?UNDEF,
    high1 = ?UNDEF,
    high2 = ?UNDEF,
    suit = ?UNDEF,
    score = 0
  }).

-record(player_hand, {
    rank = ?HC_HIGH_CARD,
    high1 = ?UNDEF,
    high2 = ?UNDEF,
    suit = ?UNDEF
  }).

-record(seat, {
    sn = ?UNDEF,
    pid = ?UNDEF,               %% player id
    process = ?UNDEF,            %% player process
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
    observers = [],             %% game observers [{pid, process}] -> proplists
    required = 2,               %% players required to start a game
    joined = 0,
    b = ?UNDEF,                 %% button
    sb = ?UNDEF,                %% small blind
    bb = ?UNDEF,                %% big blind
    sb_amt = 0,
    bb_amt = 0,
    headsup = false,
    max_betting = 0,
    exp_seat = none,            %% expecting seat
    exp_call = 0,               %% expecting call amount
    exp_min = 0,                %% expecting raise min amount
    exp_max = 0,                %% expecting raise max amount
    stage = ?UNDEF,             %% current stage
    winners = [],               %% last winners
    timer = ?UNDEF
  }).

