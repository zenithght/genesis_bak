-module(client).
-export([loop/2]).

-include("common.hrl").
-include("game.hrl").
-include("protocol.hrl").

-record(pdata, { 
    timer = ?UNDEF, 
    server = global:whereis_name(server),
    player = ?UNDEF 
  }).

loop(connected, ?UNDEF) -> 
  #pdata{timer = erlang:start_timer(?CONNECT_TIMEOUT, self(), ?MODULE)};

loop(disconnected, Data = #pdata{}) ->
  %% logout
  Data;

loop({msg, {timeout, _, ?MODULE}}, Data) ->
  self() ! close,
  Data;

loop({recv, Data}, C = #pdata{}) when is_binary(Data) ->
  loop({recv, pp:read(Data)}, C);

loop({recv, #login{}}, #pdata{player = P}) when P =:= ?UNDEF ->
  #pdata{player = new_player_process}.
