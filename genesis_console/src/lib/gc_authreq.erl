-module(gc_authreq).
-compile([export_all]).

require_login(Req, SessionID) ->
  Login = string:concat("/admin/login?uri=", Req:uri()),
  case boss_session:get_session_data(SessionID, "LOGIN") of
    undefined ->
      {redirect, Login};
    _ ->
      {ok, []}
  end.

set_login(Usr, SessionID) ->
  boss_session:set_session_data(SessionID, "LOGIN", Usr).

get_login(SessionID) ->
  boss_session:get_session_data(SessionID, "LOGIN").
