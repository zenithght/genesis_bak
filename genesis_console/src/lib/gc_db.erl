-module(gc_db).
-export([start/0]).

start() ->
  mnesia:start().
