-module(gc_agent).
-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-behavior(gen_server).

-include("common.hrl").

start_link(R = #tab_agent{identity = Id}) ->
  gen_server:start_link({local, ?GC_AGENT_NAME(Id)}, ?MODULE, [R], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Callback
%%

init([R = #tab_agent{}]) ->
  {ok, #gc_agent{}}.

terminate(Reason, _LoopData) ->
  ok.

handle_cast(stop, _LoopData) ->
  {stop, normal, _LoopData}.

handle_call(detail, _From, L = #gc_agent{}) ->
  {reply, L, L}.
