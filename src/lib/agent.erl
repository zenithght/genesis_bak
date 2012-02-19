-module(agent).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/0, create/3, auth/2]).

-include("common.hrl").
-include("schema.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(agent, {
    cash = 0,           % cash balance
    credit = 0,         % credit balance, max owe to system.
    turnover = 0,       % today turnover
    subordinate = [],   % low level agent list
    record,             % tab_agent record
    disable = false
  }).

%% Server Function

init([R = #tab_agent{}]) when R#tab_agent.disable =:= true ->
  {stop, disable_agent};
init([R = #tab_agent{}]) when R#tab_agent.disable =:= false ->
  Subordinate = setup_subordinate(R#tab_agent.identity),
  {ok, #agent{ 
      subordinate = Subordinate,
      record = R
    }
  }.

handle_cast(_Msg, Agent) ->
  {noreply, Agent}.

handle_call({auth, ReqPwd}, _From, Agent = #agent{record = R}) when
    ReqPwd /= R#tab_agent.password ->
  {reply, false, Agent};

handle_call({auth, _Pwd}, _From, Data) ->
  {reply, true, Data};

handle_call(subordinate, _From, Data) ->
  {reply, Data#agent.subordinate, Data};

handle_call(_Msg, _From, Agent) ->
  {noreply, Agent}.

handle_info(_Msg, Server) ->
  {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

terminate(normal, _Server) ->
  ok.

%% Client Function

start() ->
  Fun = fun(R = #tab_agent{identity = Identity}, _Acc) ->
      case check(list_to_atom(Identity)) of
        undefined ->
          start_agent(R);
        stopped ->
          throw({agent_stopped, Identity});
        _ ->
          ok
      end
  end, 

  ok = mnesia:wait_for_tables([tab_agent], 1000),
  {atomic, _Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], tab_agent) end).

kill() ->
  Fun = fun(R = #tab_agent{identity = Identity}, _Acc) ->
      case check(list_to_atom(Identity)) of
        Agent when is_pid(Agent) ->
          exit(Agent, kill);
        _ ->
          ok
      end
  end,
  ok = mnesia:wait_for_tables([tab_agent], 1000),
  {atomic, _Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], tab_agent) end).

start_agent(#tab_agent{identity = Identity, disable = Disable}) 
when is_list(Identity), Disable =:= true ->
  disable_agent;
start_agent(R = #tab_agent{identity = Identity, disable = Disable}) 
when is_list(Identity), Disable =:= false ->
  Name = {global, {agent, list_to_atom((Identity))}},
  {ok, Pid} = gen_server:start(Name, agent, [R], []),
  Pid.

create(Identity, Password, Parent) 
when is_atom(Parent), is_list(Identity), is_list(Password) ->
  case {check(Parent), check(Identity)} of
    {undefined, _Identity} ->
      throw(parent_undefined);
    {stopped, _Identity} ->
      throw(parent_stopped);
    {_Pid, stopped} ->
      throw(identity_stopped_and_repeat);
    {_Pid, IdentityPid} when is_pid(IdentityPid) ->
      throw(identity_repeat);
    {ParentPid, undefined} when is_pid(ParentPid) ->
      Agent = #tab_agent{ 
        aid = counter:bump(agent),
        identity = Identity,
        password = Password,
        parent = Parent
      },
      {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Agent) end),
      start_agent(Agent)
  end.
      
auth(Identity, Password) when is_list(Identity), is_list(Password) ->
  case check(Identity) of
    undefined ->
      false;
    stopped ->
      false;
    Agent when is_pid(Agent) ->
      gen_server:call(Agent, {auth, Password})
  end.

subordinate(Identity) when is_list(Identity) ->
  subordinate(list_to_existing_atom(Identity));
subordinate(Identity) when is_atom(Identity) ->
  gen_server:call(check(Identity), subordinate).

%% Private Function

setup_subordinate(Identity) when is_list(Identity) ->
  setup_subordinate(list_to_existing_atom(Identity));
setup_subordinate(Identity) when is_atom(Identity) ->
  case mnesia:dirty_index_read(tab_agent, Identity, parent) of
    Subordinate when is_list(Subordinate) ->
      Result = lists:map(fun(Agent) -> list_to_existing_atom(Agent#tab_agent.identity) end, Subordinate)
  end.

check(undefined) ->
  undefined;
check(Identity) when is_list(Identity) ->
  check(list_to_existing_atom(Identity));
check(Identity) when is_atom(Identity) ->
  check(global:whereis_name({agent, Identity}));
check(Identity) when is_pid(Identity) ->
  case is_process_alive(Identity) of
    true ->
      Identity;
    _ ->
      stopped
  end.

%% Eunit Test Case

subordinate_test() ->
  setup(),
  ?assert(is_pid(check(agent_1))),
  ?assertEqual([agent_1_1], subordinate(agent_1)),
  ?assertEqual([agent_1, disable_agent], subordinate(root)).

check_test() ->
  setup(),
  ?assert(undefined =:= check(unknown)),
  ?assert(is_pid(check(root))),
  exit(global:whereis_name({agent, root}), kill),
  ?assert(stopped =:= check(root)).

start_disable_agent_test() ->
  setup(),
  ?assertEqual(undefined, check(disable_agent)).

auth_test() ->
  setup(),
  ?assert(true =:= auth("root", "password")),
  ?assert(false =:= auth("root", "")).

create_test() ->
  setup(),
  ?assert(is_pid(create("user", "pass", root))),
  ?assert(is_pid(check(user))),
  ?assertThrow(identity_repeat, create("user", "pass", root)),
  ?assertThrow(parent_undefined, create("user", "pass", other_root)).

setup() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data(),

  Agents = [
    #tab_agent{ 
      aid = counter:bump(agent), 
      identity = "disable_agent", 
      password = "password", 
      parent = root,
      disable = true
    }, #tab_agent{
      aid = counter:bump(agent), 
      identity = "agent_1", 
      password = "password", 
      parent = root
    }, #tab_agent{
      aid = counter:bump(agent), 
      identity = "agent_1_1", 
      password = "password", 
      parent = agent_1
    }
  ],

  lists:map(fun(Agent) -> 
        {atomic, _} = mnesia:transaction( fun() -> mnesia:write(Agent) end) 
    end, Agents),

  kill(),
  start().
