-module(genesis_console_player_controller, [Req, SessionID]).
-compile(export_all).

-include_lib("genesis/include/schema.hrl").

before_(_Act) -> gc_authreq:require_login(Req, SessionID).

create('GET', []) ->
  {ok, []}.

%create('POST', []) ->
  %Errors = validation:check_request(Req, [
      %fun validation:validate_password/1,
      %fun validation:validate_repassword/1,
      %fun validation:validate_identity/1,
      %fun validation:validate_amount/1
    %], []),

  %case Errors of
    %[] ->
      %Identity = authreq:get_login(SessionID),

      %PlayerInfo = #tab_player_info {
        %nick = Req:post_param("nick"),
        %identity = Req:post_param("identity"), 
        %password = Req:post_param("password"),
        %agent = Identity,
        %cash = list_to_integer(Req:post_param("cash")),
        %credit = list_to_integer(Req:post_param("credit"))
      %},

      %Result = agent:create(Identity, PlayerInfo),

      %case Result of
        %ok ->
          %?SUCCESSFUL;
        %Error ->
          %?ERROR([Error])
      %end;
    %_ ->
      %?ERROR(Errors)
  %end.
