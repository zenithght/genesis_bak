-module(genesis_console_admin_controller, [Req, SessionID]).
-compile(export_all).

-include_lib("genesis/include/common.hrl").
-include_lib("genesis/include/schema.hrl").
-define(ROOT, "/admin/index").

before_(Act) when Act =:= "login" -> {ok, []};
before_(Act) -> gc_authreq:require_login(Req, SessionID).

index('GET', []) ->
  {ok, []}.

%user('GET', []) ->
  %{ok, []}.

login('GET', []) ->
  {ok, []};

login('POST', []) ->
  Usr = Req:post_param("username"),
  Pwd = Req:post_param("password"),

  case gc_authreq:verify_singin(Usr, Pwd) of
    true ->
      gc_authreq:set_login(Usr, SessionID),
      {redirect, get("uri", ?ROOT)};
    false ->
      {ok, []}
  end.

logout('GET', []) ->
  gc_authreq:set_login(undefined, SessionID),
  {redirect, "login"}.

get(Name, Def) ->
  case Req:query_param(Name) of
    undefined ->
      Def;
    Param ->
      Param
  end.
