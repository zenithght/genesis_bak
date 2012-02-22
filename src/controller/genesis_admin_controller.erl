-module(genesis_admin_controller, [Req, SessionID]).
-compile(export_all).

-include("../lib/schema.hrl").

before_(Act) ->
  case boss_session:get_session_data(SessionID, "LOGIN") of
    undefined when Act /= "login" ->
      {redirect, string:concat("login?act=", Act)};
    undefined when Act =:= "login" ->
      {ok, []};
    Usr ->
      {ok, []}
  end.

index('GET', []) ->
  {ok, []}.

root('GET', []) ->
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
      boss_session:set_session_data(SessionID, "LOGIN", Usr),
      {redirect, get("act", "/admin/")};
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

