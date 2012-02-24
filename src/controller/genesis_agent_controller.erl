-module(genesis_agent_controller, [Req, SessionID]).
-compile(export_all).

-define(SUCCESSFUL, {json, [{successful, true}]}).
-define(SUCCESSFUL_DATA(DataList), {json, [{successful, true}] ++ DataList}).
-define(ERROR(Errors), {json, [{successful, false}, {errors, Errors}]}).

-include("../lib/schema.hrl").

before_(_Act) -> authreq:require_login(Req, SessionID).

index('GET', []) ->
  {ok, []}.

create('GET', []) ->
  {ok, []};

create('POST', []) ->
  Errors = check_request(Req, [
      fun repeat_identity/1,
      fun validation:validate_password/1,
      fun validation:validate_repassword/1,
      fun validation:validate_identity/1
    ], []),

  case Errors of
    [] ->
      Identity = authreq:get_login(SessionID),
      Result = agent:create(Identity,
        #tab_agent {
          identity = Req:post_param("identity"), 
          password = Req:post_param("password"),
          parent = Identity
        }
      ),

      case Result of
        ok ->
          ?SUCCESSFUL;
        Error ->
          ?ERROR([Error])
      end;
    _ ->
      ?ERROR(Errors)
  end.

check_request(Req, [], Errors) -> Errors;
check_request(Req, [H|T], Errors) ->
  case H(Req) of
    ok ->
      check_request(Req, T, Errors);
    Why ->
      check_request(Req, T, [Why | Errors])
  end.

repeat_identity(Req) ->
  case mnesia:dirty_index_read(tab_agent, Req:post_param("identity"), identity) of
    [] ->
      ok;
    _ ->
      repeat_identity
  end.

balance() ->
  ?SUCCESSFUL_DATA([{balance, agent:balance(authreq:get_login(SessionID))}]).
