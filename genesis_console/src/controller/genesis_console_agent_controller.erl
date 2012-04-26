-module(genesis_console_agent_controller, [Req, SessionID]).
-compile(export_all).

-include_lib("genesis/include/schema.hrl").

%before_(_Act) -> authreq:require_login(Req, SessionID).

%index('GET', []) ->
  %{ok, []}.

%create('GET', []) ->
  %{ok, []};

%create('POST', []) ->
  %Errors = validation:check_request(Req, [
      %fun repeat_identity/1,
      %fun validation:validate_password/1,
      %fun validation:validate_repassword/1,
      %fun validation:validate_identity/1,
      %fun validation:validate_amount/1
    %], []),

  %case Errors of
    %[] ->
      %Identity = authreq:get_login(SessionID),
      %Result = agent:create(Identity,
        %#tab_agent {
          %identity = Req:post_param("identity"), 
          %password = Req:post_param("password"),
          %parent = Identity,
          %cash = list_to_integer(Req:post_param("cash")),
          %credit = list_to_integer(Req:post_param("credit"))
        %}
      %),

      %case Result of
        %ok ->
          %?SUCCESSFUL;
        %Error ->
          %?ERROR([Error])
      %end;
    %_ ->
      %?ERROR(Errors)
  %end.

%repeat_identity(Req) ->
  %case mnesia:dirty_index_read(tab_agent, Req:post_param("identity"), identity) of
    %[] ->
      %ok;
    %_ ->
      %repeat_identity
  %end.
