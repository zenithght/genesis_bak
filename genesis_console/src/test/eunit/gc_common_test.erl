-module(gc_common_test).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

common_test_() -> [
    ?_assertEqual(date(), ?DATE),
    ?_assertEqual(date(), ?DATE(0)) ].
