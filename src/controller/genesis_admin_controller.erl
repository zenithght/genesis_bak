-module(genesis_admin_controller, [Req, SessionID]).
-compile(export_all).

-include("schema.hrl").
-define(ROOT, "/admin/").

before_(Act) when Act =:= "login" -> {ok, []};
before_(Act) -> authreq:require_login(Req, SessionID).

index('GET', []) ->
  {ok, []}.

user('GET', []) ->
  {ok, []}.

login('GET', []) ->
  {ok, []};

login('POST', []) ->
  Usr = Req:post_param("username"),
  Pwd = Req:post_param("password"),

  case agent:auth(Usr, Pwd) of
    true ->
      authreq:set_login(Usr, SessionID),
      {redirect, get("uri", ?ROOT)};
    false ->
      {ok, []}
  end.

logout('GET', []) ->
  authreq:set_login(undefined, SessionID),
  {redirect, "login"}.

get(Name, Def) ->
  case Req:query_param(Name) of
    undefined ->
      Def;
    Param ->
      Param
  end.
