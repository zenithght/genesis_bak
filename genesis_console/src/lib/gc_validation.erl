-module(gc_validation).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

validate_password(Req) when not is_list(Req) ->
  validate_password(Req:post_param("password"));
validate_password(Password) ->
  case validate(Password, 6, 56, "^[a-zA-Z0-9#_@$%^&*.]+$") of
    invalidate ->
      invalidate_password;
    _ ->
      ok
  end.

validate_identity(Req) when not is_list(Req) ->
  validate_identity(Req:post_param("identity"));
validate_identity(Identity) ->
  case validate(Identity, 4, 10, "^[a-z][a-z0-9_]+[a-z]$") of
    invalidate ->
      invalidate_identity;
    _ ->
      ok
  end.

validate_repassword(Req) when not is_list(Req) ->
  validate_repassword(Req:post_param("password"), Req:post_param("re-password")).

validate_repassword(Password, RePassword) when Password =:= RePassword ->
  ok;
validate_repassword(_, _) ->
  invalidate_repassword.

validate(String, MinLen, MaxLen, _Regexp) when length(String) > MaxLen; length(String) < MinLen ->
    invalidate;
validate(String, _MinLen, _MaxLen, Regexp) ->
  case re:run(String, Regexp) of
    nomatch ->
      invalidate;
    _ ->
      ok
  end.

validate_identity_test() ->
  ?assertEqual(validate_identity("Asdf"), invalidate_identity),
  ?assertEqual(validate_identity("asdF"), invalidate_identity),
  ?assertEqual(validate_identity("_asdf"), invalidate_identity),
  ?assertEqual(validate_identity("asdf_"), invalidate_identity),
  ?assertEqual(validate_identity("as#df"), invalidate_identity),
  ?assertEqual(validate_identity("asf"), invalidate_identity),
  ?assertEqual(validate_identity("asfadfheffa"), invalidate_identity),
  ?assertEqual(validate_identity("asdf"), ok),
  ?assertEqual(validate_identity("a_bdf"), ok),
  ?assertEqual(validate_identity("asdf_a"), ok).

validate_password_test() ->
  ?assertEqual(validate_password("#_@A.$Ca%b^&*"), ok),
  ?assertEqual(validate_password([123, 4234, 322]), invalidate_password).

validate_repassword_test() ->
  ?assertEqual(validate_repassword("aa", "aa"), ok),
  ?assertEqual(validate_repassword("aa", "aac"), invalidate_repassword).

validate_amount(Req) ->
  validate_amount(Req:post_param("cash"), Req:post_param("credit")).

validate_amount(Cash, Credit) when Cash =:= undefined; Credit =:= undefined ->
    invalidate_amount;

validate_amount(Cash, Credit) when is_list(Cash), is_list(Credit) ->
  case {catch list_to_integer(Cash), catch list_to_integer(Credit)} of
    {_, {'EXIT', _}} ->
      invalidate_amount;
    {{'EXIT', _}, _} ->
      invalidate_amount;
    {CashInteger, CreditInteger}->
      validate_amount(CashInteger, CreditInteger)
  end;
  
validate_amount(Cash, Credit) when is_integer(Cash), is_integer(Credit) ->
  case {Cash, Credit} of
    {0, 0} ->
      invalidate_amount;
    {Cash, Credit} when Cash < 0; Credit < 0 ->
      invalidate_amount;
    _ ->
      ok
  end.

check_request(_Req, [], Errors) -> Errors;
check_request(Req, [H|T], Errors) ->
  case H(Req) of
    ok ->
      check_request(Req, T, Errors);
    Why ->
      check_request(Req, T, [Why | Errors])
  end.

