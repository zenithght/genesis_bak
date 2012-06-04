-module(genesis_console_agent_controller, [Req, SessionID]).
-compile(export_all).

-include_lib("genesis/include/schema.hrl").

before_(_Act) -> gc_authreq:require_login(Req, SessionID).

index('GET', []) ->
  {ok, []}.

create('GET', []) ->
  {ok, []}.

%%%
%%% Private 
%%%

%sn() ->
  %case random:seed(now()) of
    %undefined -> sn();
    %{_, _, N} -> integer_to_list(N)
  %end.

%create(Agt, Desc, Cash, Credit) 
  %when is_atom(Agent), is_integer(Cash), is_integer(Credit), is_list(Desc) ->
    %Identity = list_to_atom("agt" ++ sn()),
    %Password = os:cmd("openssl rand 6 -base64"),

    %Fun () ->
      %mnesia:lock({table, tab_agent}, write),
      %[P] = mnesia:index_read({tab_agent, R#tab_agent.aid}),

%create(Agt, Desc, 

%create('POST', []) ->
  %Errors = gc_validation:check_request(Req, [
      %fun repeat_identity/1,
      %fun gc_validation:validate_password/1,
      %fun gc_validation:validate_repassword/1,
      %fun gc_validation:validate_identity/1,
      %fun gc_validation:validate_amount/1
    %], []),

  %case Errors of
    %[] ->
      %Identity = gc_authreq:get_login(SessionID),
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
  


