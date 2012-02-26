-record(tab_agent, {
    aid,
    identity,
    password,
    root = false,
    disable = false,
    parent = root,
    subordinate = [],
    cash = 0,
    credit = 0
  }).

-record(tab_turnover, {
    aid,      %% aid
    pid,      %% pid
    game,     %% {gid, sn}
    out,      %% integer
    in,       %% integer
    inplay,   %% integer
    date,     %% {year, month, day}
    time      %% {hour, min, sec}
  }).

-record(tab_counter, {
    type,
    value
  }).

-record(tab_player_info, {
    pid,
    identity,
    password,
    nick,
    photo,
    login_errors = 0,
    disabled = false,
    agent = "root"
  }).

-record(tab_player, {
    pid                 ::integer(),
    process = undefined ::pid() | undefined,
    socket = undefined  ::pid() | undefined 
  }).

-record(tab_balance, {
    pid, 
    amount % integer
  }).

-record(tab_inplay, {
    gidpid, 
    amount % integer
  }).

-record(tab_game_xref, {
    gid,
    process,
    type,
    limit,
    table_name,
    seat_count,
    timeout,
    required % min player count 
  }).

-record(tab_game_config, {
    id,
    type,
    limit,
    seat_count,
    start_delay,
    player_timeout,
    max
  }).

-record(tab_cluster_config, {
    id,
    gateways = [],
    mnesia_masters = [],
    logdir = "/tmp",
    max_login_errors = 5,
    %% players can start games
    enable_dynamic_games = false,
    test_game_pass
  }).
