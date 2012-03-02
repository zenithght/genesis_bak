-module(client).
-export([loop/2, send/2]).

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

loop(disconnected, _Data = #pdata{}) -> ok;

loop({recv, Bin}, Data = #pdata{}) when is_binary(Bin) ->
  case catch pp:read(Data) of
    {'EXIT', {Reason, Stack}} ->
      ?LOG([{recv, Bin}, {error, {Reason, Stack}}]),
      close_connection();
    R ->
      loop({protocol, R}, Data)
  end;

% cancel connection timer when remote client first send protocol must #login
loop({protocol, R = #login{}}, Data = #pdata{timer = T}) ->
  catch erlang:cancel_timer(T),
  loop({protocol, R}, Data#pdata{timer = ?UNDEF});

loop({protocol, #login{usr = Identity, pass = Password}}, Data) ->
  case player:auth(binary_to_list(Identity), binary_to_list(Password)) of
    {ok, unauth} ->
      send(#bad{error = ?ERR_UNAUTH}),
      close_connection();
    {ok, pass} ->
      {ok, Player} = player:start(Identity),
      Data#pdata{player = Player}
  end;

loop({msg, {timeout, _, ?MODULE}}, _Data) ->
  close_connection().

%%%
%%% client
%%%

send(PID, Msg) when is_pid(PID) ->
  self() ! {send, Msg}.

%%%
%%% private
%%%

close_connection() ->
  self() ! close.

send(Msg) ->
  send(self(), {send, Msg}).
