-module(genesis_admin_controller, [Req, SessionID]).
-compile(export_all).

-include("../lib/schema.hrl").

-define(ROOT, "/admin/").

before_(Act) when Act =:= "login" ->
  case get_login_session() of
    undefined ->
      {ok, []};
    _ ->
      {redirect, ?ROOT}
  end;

before_(Act) ->
  case get_login_session() of
    undefined ->
      Url = string:concat("login?act=", Act),
      {redirect, Url};
    _ ->
      {ok, []}
  end.

index('GET', []) ->
  {ok, []}.

user('GET', []) ->
  {ok, []}.

login('GET', []) ->
  error_logger:info_report(Req:header("referer")),
  {ok, []};

login('POST', []) ->
  Usr = Req:post_param("username"),
  Pwd = Req:post_param("password"),

  case agent:auth(Usr, Pwd) of
    true ->
      set_login_session(Usr),
      {redirect, get("act", ?ROOT)};
    false ->
      {ok, []}
  end.

get(Name, Def) ->
  case Req:query_param(Name) of
    undefined ->
      Def;
    Param ->
      Param
  end.

get_login_session() ->
  boss_session:get_session_data(SessionID, "LOGIN").

set_login_session(Usr) ->
  boss_session:set_session_data(SessionID, "LOGIN", Usr).

