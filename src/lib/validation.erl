-module(validation).
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
  case regexp:match(String, Regexp) of
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
