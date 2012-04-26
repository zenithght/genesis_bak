-module(gc_db).
-export([start/0]).

-include("common.hrl").

start() ->
  mnesia:start().
