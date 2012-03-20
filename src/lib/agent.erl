-module(agent).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-export([start/0, start/1]).

-include("common.hrl").
-include("schema.hrl").

-include_lib("stdlib/include/qlc.hrl").

-record(pdata, {
    aid = 0,
    identity,
    cash = 0,
    credit = 0,
    turnover_daily = [],
    subordinate = gb_trees:empty(),   %% {identity, balance, turnover}
    players = gb_trees:empty(),       %% {identity, balance, turnover}
    disable = false,
    password = ?UNDEF
  }).

%% Server Function

init([R = #tab_agent{}]) ->
  process_flag(trap_exit, true),

  {ok, #pdata { 
    aid = R#tab_agent.aid,
    identity = R#tab_agent.identity,
    cash = R#tab_agent.cash,
    credit = R#tab_agent.credit,
    disable = R#tab_agent.disable,
    password = R#tab_agent.password,
    players = reload_players(R#tab_agent.identity),
    subordinate = reload_subordinate(R#tab_agent.identity),
    turnover_daily = reload_turnover(R#tab_agent.identity)
  }}.

handle_cast({betting, Date, Amt}, Data = #pdata{turnover_daily = Daily}) ->
  case proplists:lookup(Date, Daily) of
    none ->
      {noreply, Data#pdata{turnover_daily = Daily ++ [{Date, Amt}]}};
    {Date, Sum} ->
      {noreply, Data#pdata{turnover_daily = lists:keyreplace(Date, 1, Daily, {Date, Sum + Amt})}}
  end;
  
handle_cast(_Msg, Agent) ->
  {noreply, Agent}.

handle_call({auth, ReqPwd}, _From, Data = #pdata{password = Pwd}) ->
  case ReqPwd of
    Pwd ->
      {reply, true, Data};
    _ ->
      {reply, false, Data}
  end;

handle_call(turnover, _From, Data) ->
  {reply, Data#pdata.turnover_daily, Data};

handle_call({turnover, today}, _From, Data) ->
  case proplists:lookup(date(), Data#pdata.turnover_daily) of
    none ->
      {reply, {date(), 0}, Data};
    Today ->
      {reply, Today, Data}
  end;

%handle_call({create, R = #tab_player_info{identity = Identity}, {Credit, Cash}}, _Form, Data = #pdata{}) ->
  %transaction(fun() ->
        %ok = mnesia:write_lock_table(tab_player_info),
        %case mnesia:index_read(tab_player_info, Identity, identity) of
          %[] ->
            %PaymentData = cost(Data, Credit + Cash),
            %JoinedData = join(PaymentData, R),

            %NewRecord = R#tab_player_info{
              %pid = counter:bump(player),
              %password = erlang:md5(R#tab_player_info.password)
            %},

            %ok = mnesia:write(NewRecord),
            %ok = mnesia:write(JoinedData#pdata.record),

            %{ok, JoinedData};
          %_ ->
            %exit(repeat_identity)
        %end
    %end, Data);

%handle_call({create, R = #tab_agent{identity = Identity, parent = Parent}}, _Form, Data = #pdata{identity = Parent}) ->
  %Sum = R#tab_agent.cash + R#tab_agent.credit,
  %transaction(fun() -> 
        %ok = mnesia:write_lock_table(tab_agent),
        %case mnesia:index_read(tab_agent, Identity, identity) of
          %[] ->
            %PaymentData = cost(Data, Sum),
            %JoinedData = join(PaymentData, R),

            %NewRecord = R#tab_agent{
              %aid = counter:bump(agent),
              %password = erlang:md5(R#tab_agent.password)
            %},

            %ok = mnesia:write(NewRecord),
            %ok = mnesia:write(JoinedData#pdata.record),
            %ok = start(NewRecord),

            %{ok, JoinedData};
          %_ ->
            %exit(repeat_identity)
        %end
  %end, Data);

handle_call(_Msg, _From, Agent) ->
  {noreply, Agent}.

handle_info(_Msg, Server) ->
  {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

terminate(normal, _Server) ->
  ok.

%% Private Function

reload_players(Agent) ->
  Players = mnesia:dirty_index_read(tab_player_info, Agent, agent),
  lists:map(fun(#tab_player_info{pid = PId, identity = Identity}) -> {PId, Identity} end, Players).

reload_subordinate(Agent) ->
  Agents = mnesia:dirty_index_read(tab_agent, Agent, parent),
  lists:map(fun(#tab_agent{identity = Identity}) -> {Identity, 0} end, Agents).

reload_turnover(Agent) ->
  F = fun() ->
      Query = qlc:q(
        [{T#tab_turnover_log.date, T#tab_turnover_log.amt} || 
          T <- mnesia:table(tab_turnover_log),
          T#tab_turnover_log.aid =:= Agent, T#tab_turnover_log.date >= date_add(-30)
        ]
      ),

      qlc:e(Query) 
  end,

  {atomic, Logs} = mnesia:transaction(F),
  group_turnover_log(Logs).

group_turnover_log(Logs) -> 
  Result = group_turnover_log(Logs, ?UNDEF, ?UNDEF, []),
  case proplists:lookup(date(), Result) of
    none ->
      Result ++ [{date(), 0}];
    _ ->
      Result
  end.

group_turnover_log([], ?UNDEF, ?UNDEF, Daily) -> Daily;
group_turnover_log([], Date, Sum, Daily) -> Daily ++ [{Date, Sum}];
group_turnover_log([{Date, Amt}|T], ?UNDEF, ?UNDEF, Daily) ->
  group_turnover_log(T, Date, Amt, Daily);
group_turnover_log([{Date, Amt}|T], Date, Sum, Daily) ->
  group_turnover_log(T, Date, Amt + Sum, Daily);
group_turnover_log(L = [{_Date, _Amt}|_T], OldDate, Sum, Daily) ->
  group_turnover_log(L, ?UNDEF, ?UNDEF,  Daily ++ [{OldDate, Sum}]).
  
%cost(#pdata{cash = Cash, credit = Credit}, Amount) when (Cash + Credit) < Amount -> 
  %case mnesia:is_transaction() 
    %of true -> exit(less_balance); 
    %false -> less_balance 
  %end;

%cost(Data = #pdata{cash = Cash, record = R}, Amount) ->
  %Data#pdata{cash = Cash - Amount, record = R#tab_agent{cash = Cash - Amount}}.

%join(Data = #pdata{record = R, subordinate = Subordinate}, #tab_agent{identity = Identity}) ->
  %%% update record & pdata subordinate data.
  %NewRecord = R#tab_agent{subordinate = [ Identity | R#tab_agent.subordinate]},
  %NewTree = gb_trees:insert(Identity, nil, Subordinate),
  %Data#pdata{record = NewRecord, subordinate = NewTree};

%join(Data = #pdata{players = Players}, #tab_player_info{identity = Identity}) ->
  %%% update record & pdata players data.
  %NewTree = gb_trees:insert(Identity, nil, Players),
  %Data#pdata{players = NewTree}.

%% Client Function

start() ->
  ok = mnesia:wait_for_tables([tab_agent], 1000),
  mnesia:transaction(
    fun() -> mnesia:foldl(
          fun(R = #tab_agent{}, _) -> 
              start(R) 
          end, [], tab_agent) 
    end).

start(R = #tab_agent{identity = Identity}) ->
  gen_server:start_link(?AGENT(Identity), ?MODULE, [R], []).

create(Identity, R) ->
  gen_server:call(?AGENT(Identity), {create, R}).

create(Identity, R, {Credit, Cash}) ->
  gen_server:call(?AGENT(Identity), {create, R, {Credit, Cash}}).

auth(Identity, Password) when is_list(Identity), is_list(Password) ->
  gen_server:call(?AGENT(Identity), {auth, erlang:md5(Password)}).

betting(Identity, Date, Amt) when is_list(Identity) ->
  betting(?LOOKUP_AGENT(Identity), Date, Amt);
betting(Proc, Date, Amt) when is_pid(Proc) ->
  gen_server:cast(Proc, {betting, Date, Amt}).

turnover(Identity) when is_list(Identity) ->
  gen_server:call(?LOOKUP_AGENT(Identity), turnover).

turnover(Identity, today) when is_list(Identity) ->
  gen_server:call(?LOOKUP_AGENT(Identity), {turnover, today}).

subordinate(Identity) when is_list(Identity) ->
  gen_server:call(?AGENT(Identity), subordinate).

players(Identity) when is_list(Identity) ->
  gen_server:call(?AGENT(Identity), players).

balance(Identity) when is_list(Identity) ->
  [R] = mnesia:dirty_index_read(tab_agent, Identity, identity),
  R#tab_agent.cash + R#tab_agent.credit.

%% Eunit Test Case

%subordinate_test() ->
  %setup(),
  %?assertEqual(["agent_1_1"], subordinate("agent_1")),
  %?assertEqual(["agent_1", "disable_agent"], subordinate("root")).

%create_player_test() ->
  %setup(),
  %?assertEqual(repeat_identity, create("agent_1", #tab_player_info{identity = "player_1", agent = "agent_1"}, {0, 1000})),
  %?assertEqual(less_balance, create("agent_1", #tab_player_info{identity = "player_new", agent = "agent_1"}, {1, 10000})),
  %?assertEqual(10000, balance("agent_1")),
  %?assertEqual(ok, create("agent_1", #tab_player_info{identity = "player_new", agent = "agent_1", password = "def_pwd"}, {1000, 1000})),
  %?assertEqual(8000, balance("agent_1")),
  %?assertEqual(["player_3", "player_new"], players("agent_1")),
  %[NewPlayer] = mnesia:dirty_index_read(tab_player_info, "player_new", identity),
  %?assertEqual(0, NewPlayer#tab_player_info.cash).

%create_test() ->
  %setup(),
  %?assertEqual(repeat_identity, create("agent_1", #tab_agent{identity = "agent_1_1", password = ?DEF_PWD, parent = "agent_1"})),
  %?assertEqual(less_balance, create("agent_1", #tab_agent{identity = "agent_1_new", password = ?DEF_PWD, parent = "agent_1", cash = 1000 * 10000, credit = 0})),

  %?assertEqual(10000, balance("agent_1")),
  %?assertEqual(ok, create("agent_1", #tab_agent{identity = "agent_1_new", password = ?DEF_PWD, parent = "agent_1", cash = 1000, credit = 1000})),
  %?assertEqual(8000, balance("agent_1")),
  %?assert(is_pid(?LOOKUP_AGENT("agent_1_new"))),
  %?assertEqual(["agent_1_1", "agent_1_new"], subordinate("agent_1")).

%players_test() ->
  %setup(),
  %?assertEqual(["player_1", "player_2"], players("root")),
  %?assertEqual(["player_3"], players("agent_1")),
  %?assertEqual(["player_4"], players("agent_1_1")).

%betting_test() ->
  %setup(),
  %?assertEqual(not_own_player, betting("root", "player_3", 10)),
  %?assertEqual(ok, betting("root", "player_1", 10)),
  %?assertEqual(ok, betting("root", "player_2", 10)),
  %?assertEqual(ok, betting("root", "player_2", 30)),
  %?assertEqual(50, turnover("root", today)).

%auth_test() ->
  %setup(),
  %?assert(true =:= auth("root", ?DEF_PWD)),
  %?assert(false =:= auth("root", "")).

%balance_test() ->
  %setup(),
  %?assertEqual(10000, balance("agent_1")).

date_add(N) ->
  Days = calendar:date_to_gregorian_days(date()),
  calendar:gregorian_days_to_date(Days + N).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
  setup(),
  agent:start(),
  ?assert(is_pid(?LOOKUP_AGENT("root"))),
  ?assert(is_pid(?LOOKUP_AGENT("agent1"))),
  ?assert(is_pid(?LOOKUP_AGENT("agent2"))),
  ?assert(is_pid(?LOOKUP_AGENT("agent3"))).

generate_test() ->
  Agents = [#tab_agent{
      aid = Id, 
      identity = "agent" ++ integer_to_list(Id), 
      password = ?DEF_HASH_PWD,
      parent = Parent,
      disable = Disable} 
    || Id <- lists:seq(1,3), Parent <- ["root"], Disable <- [false]],

  [H|_] = Agents,
  ?assertEqual(3, length(Agents)),
  ?assertMatch(#tab_agent{aid = 1, parent = "root", disable = false, identity = "agent1"}, H),
  ?assertMatch(#tab_agent{aid = 3, parent = "root", disable = false, identity = "agent3"}, lists:last(Agents)).

group_turnover_log_test() ->
  Date = date(),
  Data = [{Date, Amt} || Date <- [{2012, 1, 1}, {2012, 1, 2}], Amt <- [5, 10]],
  ?assertMatch([{{2012, 1, 1}, 15}, {{2012, 1, 2}, 15}, {Date, 0}], group_turnover_log(Data)),
  ?assertMatch([{{2012, 1, 1}, 15}, {{2012, 1, 2}, 15}, {{2012, 1, 3}, 5}, {Date, 0}], group_turnover_log(Data ++ [{{2012, 1, 3}, 5}])).

reload_turnover_log_empty_test() ->
  schema:init(),
  DateNow = date(),
  ?assertMatch([{DateNow, 0}], reload_turnover("root")).

reload_turnover_log_test() ->
  schema:init(),
  Logs = [#tab_turnover_log{aid = "root", date = Date, amt = Amt} || Date <- [date_add(-40), date_add(-10)], Amt <- [10, 5]],
  Logs1 = Logs ++ [#tab_turnover_log{aid = "test", date = date_add(-10), amt = 10}], %% add other user check aid condition
  Logs2 = Logs1 ++ [#tab_turnover_log{aid = "root", date = date_add(-1), amt = 10}],
  lists:map(fun(R) -> mnesia:dirty_write(R) end, Logs2),
  
  DateNow = date(),
  DateBeforTen = date_add(-10),
  DateBeforOne = date_add(-1),

  ?assertMatch([{DateBeforOne, 10}, {DateBeforTen, 15}, {DateNow, 0}], reload_turnover("root")).

betting_turnover_today_test() ->
  setup(),
  agent:start(),

  Date = date(),
  BeforDate = date_add(-1),

  ?assertMatch({Date, 0}, turnover("root", today)),
  betting("root", Date, 10),
  ?assertMatch({Date, 10}, turnover("root", today)),
  ?assertMatch([{Date, 10}], turnover("root")),
  betting("root", BeforDate, 10),
  ?assertMatch({Date, 10}, turnover("root", today)),
  ?assertMatch([{Date, 10}, {BeforDate, 10}], turnover("root")).

setup() ->
  schema:init(),

  Agents = [#tab_agent{
      aid = Id, 
      identity = "agent" ++ integer_to_list(Id rem 10), 
      password = ?DEF_HASH_PWD,
      parent = Parent,
      disable = Disable} 
    || Id <- lists:seq(11, 13), Parent <- ["root"], Disable <- [false]],

  Players = [#tab_player_info{
      pid = Id,
      identity = "player" ++ integer_to_list(Id),
      password = ?DEF_HASH_PWD,
      agent = "agent" ++ integer_to_list(AId)} 
    || Id <- lists:seq(1,3), AId <- lists:seq(11,13)],

  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Agents),
  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Players).
