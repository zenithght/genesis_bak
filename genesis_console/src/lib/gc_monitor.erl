-module(gc_monitor).
-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([log_counter/0]).
-behavior(gen_server).

-record(pdata, {
    log_counter
  }).

-include("common.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Callback
%%

init([]) ->
  gc_db:monitor(tab_turnover_log),
  {ok, #pdata{log_counter = 0}}.

terminate(Reason, _LoopData) ->
  ok.

handle_call(log_counter, _From, Pd) ->
  {reply, Pd#pdata.log_counter, Pd};

handle_call(Msg, _From, _LoopData) ->
  {reply, ok, _LoopData}.

handle_cast(stop, _LoopData) ->
  {stop, normal, _LoopData}.

handle_info({mnesia_table_event, {write, #tab_turnover_log{pid = PId, date = Date, amt = Amt}, _}}, Pd) ->
  Identity = gc_db:lookup_agent(PId),
  gc_agent:log_turnover(Identity, Date, Amt),
  {noreply, Pd#pdata{log_counter = Pd#pdata.log_counter + 1}}.

%%%
%%% Public 
%%%

log_counter() ->
  gen_server:call(gc_monitor, log_counter).
