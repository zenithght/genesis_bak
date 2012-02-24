-module(agent).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/0, create/2, create/3, auth/2]).

-include("common.hrl").
-include("schema.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(AGENT(Name), {global, {agent, Name}}).
-define(LOOKUP_AGENT(Name), global:whereis_name({agent, Name})).
-define(DEF_PWD, erlang:md5("password")).

-record(pdata, {
    aid = 0,
    identity,
    cash = 0,
    credit = 0,
    turnover = 0,       % today turnover
    subordinate = gb_trees:empty(),   % low level agent list
    players = gb_trees:empty(),
    record,             % tab_agent record
    disable = false
  }).

%% Server Function

init([R = #tab_agent{}]) ->
  Agent = #pdata { 
    aid = R#tab_agent.aid,
    identity = R#tab_agent.identity,
    cash = R#tab_agent.cash,
    credit = R#tab_agent.credit,
    disable = R#tab_agent.disable,
    players = setup_players(R#tab_agent.identity),
    subordinate = lists:foldl(fun(Sub, Acc) -> gb_trees:insert(Sub, 0, Acc) end, gb_trees:empty(), R#tab_agent.subordinate),
    record = R
  },
  {ok, Agent}.

handle_cast(_Msg, Agent) ->
  {noreply, Agent}.

handle_call({turnover, today}, _From, Data) ->
  {reply, lists:foldl(fun(Val, Sum) -> Val + Sum end, 0, gb_trees:values(Data#pdata.players)), Data};

handle_call({balance, today}, _From, Data) ->
  {reply, lists:foldl(fun(Val, Sum) -> Val + Sum end, 0, gb_trees:values(Data#pdata.players)), Data};

handle_call(kill, _From, Data) ->
  {stop, normal, ok, Data};

handle_call({create, R = #tab_player_info{identity = Identity}, {Credit, Cash}}, _Form, Data = #pdata{}) ->
  transaction(fun() ->
        ok = mnesia:write_lock_table(tab_player_info),
        case mnesia:index_read(tab_player_info, Identity, identity) of
          [] ->
            PaymentData = cost(Data, Credit + Cash),
            JoinedData = join(PaymentData, R),

            NewRecord = R#tab_player_info{
              pid = counter:bump(player),
              password = erlang:md5(R#tab_player_info.password)
            },

            ok = mnesia:write(NewRecord),
            ok = mnesia:write(JoinedData#pdata.record),
            db:update_balance(tab_balance, NewRecord#tab_player_info.pid, Cash),

            {ok, JoinedData};
          _ ->
            exit(repeat_identity)
        end
    end, Data);

handle_call({create, R = #tab_agent{identity = Identity, parent = Parent}}, _Form, Data = #pdata{identity = Parent}) ->
  Sum = R#tab_agent.cash + R#tab_agent.credit,
  transaction(fun() -> 
        ok = mnesia:write_lock_table(tab_agent),
        case mnesia:index_read(tab_agent, Identity, identity) of
          [] ->
            PaymentData = cost(Data, Sum),
            JoinedData = join(PaymentData, R),

            NewRecord = R#tab_agent{
              aid = counter:bump(agent),
              password = erlang:md5(R#tab_agent.password)
            },

            ok = mnesia:write(NewRecord),
            ok = mnesia:write(JoinedData#pdata.record),
            ok = start(Identity, NewRecord),

            {ok, JoinedData};
          _ ->
            exit(repeat_identity)
        end
  end, Data);

handle_call({betting, Player, Bet}, _From, Data) when is_list(Player), is_integer(Bet), Bet > 0 ->
  case gb_trees:lookup(Player, Data#pdata.players) of
    none ->
      {reply, not_own_player, Data};
    {value, Val} ->
      {reply, ok, Data#pdata{players = gb_trees:update(Player, Val + Bet, Data#pdata.players)}}
  end;

handle_call({auth, ReqPwd}, _From, Agent = #pdata{record = R}) when
    ReqPwd /= R#tab_agent.password ->
  {reply, false, Agent};

handle_call({auth, _Pwd}, _From, Data) ->
  {reply, true, Data};

handle_call(subordinate, _From, Data) ->
  {reply, gb_trees:keys(Data#pdata.subordinate), Data};

handle_call(players, _From, Data) ->
  {reply, gb_trees:keys(Data#pdata.players), Data};

handle_call(_Msg, _From, Agent) ->
  {noreply, Agent}.

handle_info(_Msg, Server) ->
  {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

terminate(normal, _Server) ->
  ok.

%% Private Function

transaction(Fun, Data) ->
  case mnesia:transaction(Fun) of
    {atomic, {Result, NewData}} ->
      {reply, Result, NewData};
    {aborted, Ex} when is_atom(Ex) ->
      {reply, Ex, Data};
    {aborted, Ex} ->
      error_logger:error_report(Ex),
      {reply, unknown_exception, Data}
  end.

setup_players(Identity) when is_list(Identity) ->
  case mnesia:dirty_index_read(tab_player_info, Identity, agent) of
    Players when is_list(Players) ->
      Fun = fun(Player, PlayersTree) -> 
          gb_trees:insert(Player#tab_player_info.identity, 0, PlayersTree) 
      end, 
      lists:foldl(Fun, gb_trees:empty(), Players)
  end.

cost(#pdata{cash = Cash, credit = Credit}, Amount) when (Cash + Credit) < Amount -> 
  error_logger:error_report({Cash, Credit, Amount}),
  case mnesia:is_transaction() 
    of true -> exit(less_balance); 
    false -> less_balance 
  end;
cost(Data = #pdata{cash = Cash, record = R}, Amount) ->
  error_logger:error_report({Cash, Amount}),
  Data#pdata{cash = Cash - Amount, record = R#tab_agent{cash = Cash - Amount}}.

join(Data = #pdata{record = R, subordinate = Subordinate}, #tab_agent{identity = Identity}) ->
  %% update record & pdata subordinate data.
  NewRecord = R#tab_agent{subordinate = [ Identity | R#tab_agent.subordinate]},
  NewTree = gb_trees:insert(Identity, nil, Subordinate),
  Data#pdata{record = NewRecord, subordinate = NewTree};

join(Data = #pdata{players = Players}, #tab_player_info{identity = Identity}) ->
  %% update record & pdata players data.
  NewTree = gb_trees:insert(Identity, nil, Players),
  Data#pdata{players = NewTree}.

%% Client Function

start() ->
  Fun = fun(R = #tab_agent{identity = Identity}, _Acc) when is_list(Identity) ->
      case ?LOOKUP_AGENT(Identity) of
        undefined -> ok = start(Identity, R);
        A -> ?LOG({live_agent, Identity, A})
      end
  end, 

  ok = mnesia:wait_for_tables([tab_agent], 1000),
  {atomic, _Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, nil, tab_agent) end).

start(Identity, R) ->
  case gen_server:start(?AGENT(Identity), agent, [R], []) of
    {ok, _Pid} -> ok
  end.

kill() ->
  Fun = fun(#tab_agent{identity = Identity}, _Acc) ->
      case ?LOOKUP_AGENT(Identity) of
        Pid when is_pid(Pid) -> 
          ok = gen_server:call(Pid, kill);
        _ -> ok
      end
  end,

  {atomic, _Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], tab_agent) end).

create(Identity, R) ->
  gen_server:call(?AGENT(Identity), {create, R}).

create(Identity, R, {Credit, Cash}) ->
  gen_server:call(?AGENT(Identity), {create, R, {Credit, Cash}}).

auth(Identity, Password) when is_list(Identity), is_list(Password) ->
  gen_server:call(?AGENT(Identity), {auth, erlang:md5(Password)}).

betting(Identity, Player, Bet) when is_list(Identity), is_list(Player), is_integer(Bet), Bet > 0 ->
  gen_server:call(?AGENT(Identity), {betting, Player, Bet}).

turnover(Identity, today) when is_list(Identity) ->
  gen_server:call(?AGENT(Identity), {turnover, today}).

subordinate(Identity) when is_list(Identity) ->
  gen_server:call(?AGENT(Identity), subordinate).

players(Identity) when is_list(Identity) ->
  gen_server:call(?AGENT(Identity), players).

balance(Identity) when is_list(Identity) ->
  [R] = mnesia:dirty_index_read(tab_agent, Identity, identity),
  R#tab_agent.cash + R#tab_agent.credit.
  
%% Eunit Test Case

subordinate_test() ->
  setup(),
  ?assertEqual(["agent_1_1"], subordinate("agent_1")),
  ?assertEqual(["agent_1", "disable_agent"], subordinate("root")).

create_player_test() ->
  setup(),
  ?assertEqual(repeat_identity, create("agent_1", #tab_player_info{identity = "player_1", agent = "agent_1"}, {0, 1000})),
  ?assertEqual(less_balance, create("agent_1", #tab_player_info{identity = "player_new", agent = "agent_1"}, {1, 10000})),
  ?assertEqual(10000, balance("agent_1")),
  ?assertEqual(ok, create("agent_1", #tab_player_info{identity = "player_new", agent = "agent_1"}, {1000, 1000})),
  ?assertEqual(8000, balance("agent_1")),
  ?assertEqual(["player_3", "player_new"], players("agent_1")),
  [NewPlayer] = mnesia:dirty_index_read(tab_player_info, "player_new", identity),
  [Balance] = mnesia:dirty_read(tab_balance, NewPlayer#tab_player_info.pid),
  ?assertEqual(1000 * 10000, Balance#tab_balance.amount).

create_test() ->
  setup(),
  ?assertEqual(repeat_identity, create("agent_1", #tab_agent{identity = "agent_1_1", password = ?DEF_PWD, parent = "agent_1"})),
  ?assertEqual(less_balance, create("agent_1", #tab_agent{identity = "agent_1_new", password = ?DEF_PWD, parent = "agent_1", cash = 1000 * 10000, credit = 0})),

  ?assertEqual(10000, balance("agent_1")),
  ?assertEqual(ok, create("agent_1", #tab_agent{identity = "agent_1_new", password = ?DEF_PWD, parent = "agent_1", cash = 1000, credit = 1000})),
  ?assertEqual(8000, balance("agent_1")),
  ?assert(is_pid(?LOOKUP_AGENT("agent_1_new"))),
  ?assertEqual(["agent_1_1", "agent_1_new"], subordinate("agent_1")).

players_test() ->
  setup(),
  ?assertEqual(["player_1", "player_2"], players("root")),
  ?assertEqual(["player_3"], players("agent_1")),
  ?assertEqual(["player_4"], players("agent_1_1")).

betting_test() ->
  setup(),
  ?assertEqual(not_own_player, betting("root", "player_3", 10)),
  ?assertEqual(ok, betting("root", "player_1", 10)),
  ?assertEqual(ok, betting("root", "player_2", 10)),
  ?assertEqual(ok, betting("root", "player_2", 30)),
  ?assertEqual(50, turnover("root", today)).

auth_test() ->
  setup(),
  ?assert(true =:= auth("root", "password")),
  ?assert(false =:= auth("root", "")).

balance_test() ->
  setup(),
  ?assertEqual(10000, balance("agent_1")).

setup() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data(),

  DefPwd = ?DEF_PWD,

  Agents = [
    #tab_agent{ 
      aid = counter:bump(agent),
      identity = "disable_agent", 
      password = DefPwd,
      parent = "root",
      disable = true
    }, #tab_agent{
      aid = counter:bump(agent),
      identity = "agent_1", 
      password = DefPwd,
      parent = "root",
      subordinate = ["agent_1_1"],
      cash = 0,
      credit = 10000
    }, #tab_agent{
      aid = counter:bump(agent),
      identity = "agent_1_1",
      password = DefPwd,
      parent = "agent_1"
    }, #tab_agent{
      aid = counter:bump(agent),
      identity = "root",
      password = DefPwd,
      parent = nil,
      subordinate = ["disable_agent", "agent_1"]
    }
  ],

  Players = [
    #tab_player_info {
      pid = counter:bump(player),
      identity = "player_1",
      agent = "root"
    }, #tab_player_info {
      pid = counter:bump(player),
      identity = "player_2",
      agent = "root"
    }, #tab_player_info {
      pid = counter:bump(player),
      identity = "player_3",
      agent = "agent_1"
    }, #tab_player_info {
      pid = counter:bump(player),
      identity = "player_4",
      agent = "agent_1_1"
    }
  ],

  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Agents),
  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Players),

  kill(),
  start().
