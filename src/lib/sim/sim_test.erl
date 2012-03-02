-module(sim_test).

-include("common.hrl").
-include("protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

connection_timeout_test() ->
  start(player),
  ?assertEqual(true, is_pid(whereis(player))),
  timer:sleep(?CONNECT_TIMEOUT + 500),
  ?assertEqual(undefined, whereis(player)).

%%%
%%% private
%%%

start(Id) ->
  sim_client:start(Id).
  
