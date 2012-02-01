-module(agent).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/0, create/3]).

-include("common.hrl").
-include("schema.hrl").

-record(agent, 
  {
    record
  }
).

%% Server Function

init([R = #tab_agent{}]) ->
  ?LOG([{agent, R#tab_agent.username, start}]),
  {ok, #agent{record = R}}.

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
  db:start(),
  Fun = fun(Agent = #tab_agent{username = Usr}) ->
      Reg = {global, {agent, list_to_atom(binary_to_list(Usr))}},
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

create(Usr, Pwd, Parent) when 
    is_binary(Usr), 
    is_binary(Pwd), 
    is_number(Parent), 
    Usr /= <<"root">> ->
  case db:index_read(tab_agent, Usr, username) of
    [_] ->
      repeat_error;
    _ ->
      Agent = #tab_agent{ 
        aid = counter:bump(agent), 
        username = Usr, 
        password = Pwd, 
        root = 0,
        parent = Parent
      },
      db:write(Agent),
      ok
  end;

create(_, _, _) ->
  unknown_error.
