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

init([Agent = #tab_agent{}]) ->
  ?LOG([{agent, Agent#tab_agent.username, start}]),
  {ok, #agent{record = Agent}}.

handle_call(_Msg, _From, Server) ->
  {noreply, Server}.

handle_cast(_Msg, Server) ->
  {noreply, Server}.

handle_info(_Msg, Server) ->
  {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

terminate(normal, Server) ->
  ok.

%% Client Function

start() ->
  Fun = fun(Agent = #tab_agent{username = Usr}) ->
      Reg = {global, {agent, list_to_atom(binary_to_list(Usr))}},
      R = gen_server:start(Reg, agent, [Agent], []),
      ?LOG([{agent_proc, R}])
  end, 

  {atomic, Agents} = db:find(tab_agent),
  lists:map(Fun, Agents),
  ok.

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
