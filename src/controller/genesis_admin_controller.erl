-module(genesis_admin_controller, [Req, SessionID]).
-compile(export_all).

-include("../lib/schema.hrl").

before_(Act) when Act /= "login" ->
  case boss_session:get_session_data(SessionID, "LOGIN") of
    undefined ->
      {redirect, string:concat("login?act=", Act)};
    Usr ->
      {ok, []}
  end;

before_(_) ->
  {ok, []}.

root('GET', []) ->
  {ok, []}.

user('GET', []) ->
  {ok, []}.

login('GET', []) ->
  error_logger:info_report(Req:header("referer")),
  {ok, []};

login('POST', []) ->
  Usr = list_to_binary(Req:post_param("username")),
  Pwd = list_to_binary(Req:post_param("password")),

  Act = case Req:query_param("act") of
    undefined ->
      "root";
    Action ->
      Action
  end,

  case global:whereis_name({agent, list_to_atom(Req:post_param("username"))}) of
    undefined ->
      {ok, []};
    Pid -> 
      case gen_server:call(Pid, {auth, Pwd}) of
        true ->
          boss_session:set_session_data(SessionID, "LOGIN", Usr),
          {redirect, Act};
        false ->
          {ok, []}
      end
  end.
