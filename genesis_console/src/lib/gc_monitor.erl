-module(gc_monitor).
-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-behavior(gen_server).

-include("common.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Callback
%%

init([]) ->
  {ok, null}.

terminate(Reason, _LoopData) ->
  ?LOG([{gc_monitor, Reason}]).

handle_cast(stop, _LoopData) ->
  {stop, normal, _LoopData};

handle_cast(Msg, _LoopData) ->
  ?LOG([{gc_monitor, cast}, {msg, Msg}]),
  {noreply, _LoopData}.

handle_call(Msg, _From, _LoopData) ->
  {reply, ok, _LoopData}.
