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
    xref = gb_tree:empty(),     %% player to seat cross-reference
    pot = pot:new(),            %% pot structure
    deck = deck:new(),          %% card deck
    board = [],                 %% shared cards list
    observers = [],             %% game observers
    required = 2,               %% players required to start a game
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

