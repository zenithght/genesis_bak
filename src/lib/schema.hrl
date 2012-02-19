-define(ROOT_AID, 1).

-record(tab_agent, {
    aid,
    identity,
    password,
    root = false,
    disable = false,
    parent = root,
    cash = 0,
    credit = 0
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
    location,
    login_errors = 0,
    disabled = false, % player is disabled
    agent = root
  }).

-record(tab_player, {
    pid,
    process = none, % process id
    socket = none
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
    seat_count,
    limit,
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
