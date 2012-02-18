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

init([R = #tab_agent{}]) when R#tab_agent.disable /= true ->
  {ok, #agent{ record = R}}.

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

terminate(normal, _Server) ->
  ok.

%% Client Function

start() ->
  Fun = fun(R = #tab_agent{identity = Identity, disable = Disable}, _Acc) 
    when Disable =:= false ->
      Name = {global, {agent, list_to_atom(binary_to_list(Identity))}},
      gen_server:start(Name, agent, [R], [])
  end, 
  ok = mnesia:wait_for_tables([tab_agent], 1000),
  {atomic, _Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], tab_agent) end).

create(Identity, Password, _Parent) when not is_binary(Identity), not is_binary(Password) -> error;
create(Identity, Password, Parent) when is_atom(Parent) ->
  case {check(Parent), check(Identity)} of
    {_Pid, Pid} when is_pid(Pid) ->
      repeat_error;
    {Pid, undefined} when is_pid(Pid) ->
      {atomic, _Result} = mnesia:transaction(
        fun() ->
            mnesia:write(#tab_agent{ 
                aid = counter:bump(agent),
                identity = Identity,
                password = Password,
                parent = Parent
              }
            )
        end
      ),
      ok;
    _ ->
      error
  end.
      
  %case check(Identity) of
    %undefined ->
      %Agent = #tab_agent{ 
        %identity = Identity, 
        %password = Password, 
        %root = 0,
        %parent = Parent
      %},
      %db:write(Agent),
      %ok;
    %_ ->
      %repeat_error
  %end;

auth(Identity, Password) when is_list(Identity), is_list(Password) ->
  auth(list_to_binary(Identity), list_to_binary(Password));
auth(Identity, Password) when is_binary(Identity), is_binary(Password) ->
  gen_server:call(check(Identity), {auth, Password}).

%% Private Function

check(undefined) ->
  undefined;
check(Identity) when is_binary(Identity) ->
  check(list_to_atom(binary_to_list(Identity)));
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

check_test() ->
  setup(),
  ?assert(undefined =:= check(unknown)),
  ?assert(is_pid(check(root))),
  exit(global:whereis_name({agent, root}), kill),
  ?assert(stopped =:= check(root)).
  
auth_test() ->
  setup(),
  ?assert(true =:= auth("root", "password")),
  ?assert(true =:= auth(<<"root">>, <<"password">>)),
  ?assert(false =:= auth("root", "")),
  ?assert(false =:= auth(<<"root">>, <<"">>)).

create_test() ->
  setup(),
  ?assert(ok =:= create(<<"user">>, <<"pass">>, root)),
  ?assert(error =:= create("user", "pass", root)),
  ?assert(repeat_error =:= create(<<"root">>, <<"pass">>, root)).

setup() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data(),
  start().
