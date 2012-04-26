-module(gc_authreq).
-compile([export_all]).

-include_lib("genesis/include/common.hrl").
-include_lib("genesis/include/schema.hrl").

require_login(Req, SessionID) ->
  {_, Uri} = Req:uri(),
  RedirectUri = string:concat("/admin/login?uri=", Uri),

  case boss_session:get_session_data(SessionID, "LOGIN") of
    undefined ->
      {redirect, RedirectUri};
    _ ->
      {ok, []}
  end.

set_login(Usr, SessionID) ->
  boss_session:set_session_data(SessionID, "LOGIN", Usr).

get_login(SessionID) ->
  boss_session:get_session_data(SessionID, "LOGIN").

verify_singin(Usr, Pwd) ->
  ok = mnesia:wait_for_tables([tab_agent], 2000),
  case mnesia:dirty_index_read(tab_agent, Usr, identity) of
    [] ->
      false;
    [Agent] ->
      erlang:md5(Pwd) =:= Agent#tab_agent.password
  end.
