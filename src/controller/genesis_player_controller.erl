-module(genesis_player_controller, [Req, SessionID]).
-compile(export_all).

-include("controller.hrl").
-include("schema.hrl").

before_(_Act) -> authreq:require_login(Req, SessionID).

create('GET', []) ->
  {ok, []};

create('POST', []) ->
  Errors = validation:check_request(Req, [
      fun repeat_identity/1,
      fun validation:validate_password/1,
      fun validation:validate_repassword/1,
      fun validation:validate_identity/1,
      fun validation:validate_amount/1
    ], []),

  case Errors of
    [] ->
      Identity = authreq:get_login(SessionID),

      PlayerInfo = #tab_player_info {
        nick = Req:post_param("nick"),
        identity = Req:post_param("identity"), 
        password = Req:post_param("password"),
        agent = Identity
      },

      Cash = list_to_integer(Req:post_param("cash")),
      Credit = list_to_integer(Req:post_param("credit")),

      Result = agent:create(Identity, PlayerInfo, {Credit, Cash}),

      case Result of
        ok ->
          ?SUCCESSFUL;
        Error ->
          ?ERROR([Error])
      end;
    _ ->
      ?ERROR(Errors)
  end.

repeat_identity(Req) ->
  case mnesia:dirty_index_read(tab_player_info, Req:post_param("identity"), identity) of
    [] ->
      ok;
    _ ->
      repeat_identity
  end.
