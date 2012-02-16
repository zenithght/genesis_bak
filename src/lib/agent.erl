-module(agent).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/0, create/3]).

-include("common.hrl").
-include("schema.hrl").

-record(agent, {
    credit = 0,    % 信用额度，账户最大可赊欠的数额。
    balance = 0,   % 账户的现金余额，可使用信用额度进行赊欠。
    turnover = 0,  % 当日流水额
    
    players = [],      % 下级玩家列表
    subordinate = [],  % 下级代理列表
    disable = false,   % 代理是否被停用

    record     % 数据库中的原始记录
  }).

%% Server Function

init([Identity]) ->
  case check(Identity) of
    nil ->
      {stop};
    Agent ->
      init_agent(Agent)
  end.

handle_cast(_Msg, Agent) ->
  {noreply, Agent}.

handle_call({auth, ReqPwd}, _From, Agent = #agent{record = R}) when
    ReqPwd /= R#tab_agent.password ->
  {reply, false, Agent};

handle_call({auth, _Pwd}, _From, Agent) ->
  {reply, true, Agent};

handle_call(_Msg, _From, Agent) ->
  {noreply, Agent}.

handle_info(_Msg, Server) ->
  {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

terminate(normal, Server) ->
  ok.

%% Client Function

start() ->
  Fun = fun(Agent = #tab_agent{identity = Identity}) ->
      Reg = {global, {agent, list_to_atom(binary_to_list(Identity))}},
      R = gen_server:start(Reg, agent, [Agent], []),
      ?LOG([{agent_proc, R}])
  end, 

  case db:wait_for_tables([tab_agent], 10000) of 
    ok ->
      {atomic, Agents} = db:find(tab_agent),
      lists:map(Fun, Agents);
    _ ->
      {error}
  end.

create(Identity, Password, Parent) when 
    is_binary(Identity), 
    is_binary(Password), 
    is_number(Parent), 
    Identity /= <<"root">> ->
  case check(Identity) of
    nil ->
      Agent = #tab_agent{ 
        identity = Identity, 
        password = Password, 
        root = 0,
        parent = Parent
      },
      db:write(Agent),
      ok;
    _ ->
      repeat_error
  end;

create(_, _, _) ->
  unknown_error.

%% Private Function

init_agent(R = #tab_agent{ balance = Balance, credit = Credit, disable = Disable }) ->
  {ok, #agent{
      balance = Balance,
      credit = Credit,
      disable = Disable,
      record = R
    }
  }.

check(Identity) ->
  case db:index_read(tab_agent, Identity, identity) of
    [Agent] ->
      Agent;
    _ ->
      nil
  end.
