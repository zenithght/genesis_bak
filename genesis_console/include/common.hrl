-include_lib("genesis/include/common.hrl").
-include_lib("genesis/include/schema.hrl").
-include("gc_agent.hrl").

-define(GC_AGENT_NAME(Identity), erlang:list_to_atom("gc_" ++ atom_to_list(Identity) ++ "_agent")).
-define(GC_COLLECT_TIME, 1000 * 60).
-define(GC_ROOT_LEVEL, 0).

-define(SPAWN_TEST(Tests), {spawn, {setup, fun setup/0, fun cleanup/1, Tests}}).
-define(SLEEP, timer:sleep(200)).

-define(DATE(Day), calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date()) + Day)).
-define(DATE, ?DATE(0)).
