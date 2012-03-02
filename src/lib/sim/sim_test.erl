-module(sim_test).

-include("common.hrl").
-include("protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SLEEP, timer:sleep(500)).

connection_timeout_test() ->
  run_by_def(fun() ->
        ?assertEqual(true, is_pid(where())),

        timer:sleep(?CONNECT_TIMEOUT + 500),

        [E] = sim_client:box(),
        ?assertEqual(undefined, where()),
        ?assertEqual(E#bad.error, ?ERR_CONNECTION_TIMEOUT)
    end).

login_timeout_test() ->
  run_by_def(fun() ->
        ?assertEqual(true, is_pid(where())),
        send(#login{usr = <<"player">>, pass = <<"def_pwd">>}),

        ?SLEEP,
        
        [E] = sim_client:box(),
        ?assertEqual(undefined, where()),
        ?assertEqual(E#bad.error, ?ERR_UNAUTH)
    end).

%%%
%%% private
%%%

flush() ->
	receive 
		_Any ->
			flush()
	after 
		0 ->
			true
	end.

run_by_def(Fun) ->
  flush(),
  catch player ! kill,
  sim_client:start(player),
  setup(),
  Fun().

where() ->
  whereis(player).

send(R) ->
  player ! {send, R}.

setup() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data().
