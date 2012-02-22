-module(genesis_player_controller, [Req, SessionID]).
-compile(export_all).

before_(Act) -> authreq:require_login(Req, SessionID).

index('GET', []) ->
  {ok, []}.

create('GET', []) ->
  {ok, []};
create('POST', []) ->
  {ok, []}.
