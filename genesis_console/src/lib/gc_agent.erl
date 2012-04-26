-module(gc_agent).
-export([start_link/2, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-behavior(gen_server).

-include("common.hrl").

start_link(R = #tab_agent{}, Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [R], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Callback
%%

init([R = #tab_agent{}]) ->
  {ok, R}.

terminate(Reason, _LoopData) ->
  ok.

handle_cast(stop, _LoopData) ->
  {stop, normal, _LoopData}.

handle_call(id, _From, LoopData) ->
  {reply, LoopData#tab_agent.identity, LoopData};

handle_call(Msg, _From, _LoopData) ->
  {reply, ok, _LoopData}.
