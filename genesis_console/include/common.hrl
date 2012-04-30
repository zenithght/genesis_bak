-include_lib("genesis/include/common.hrl").
-include_lib("genesis/include/schema.hrl").
-include("gc_agent.hrl").

-define(GC_AGENT_NAME(Identity), erlang:list_to_atom("gc_" ++ Identity ++ "_agent")).
