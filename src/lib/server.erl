-module(server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, start/2, stop/0]).

-include("common.hrl").
-include("protocol.hrl").
-include("schema.hrl").

-record(pdata, { port, host }).
-record(client, { server = global:whereis_name(server), player = ?UNDEF }).

%%%
%%% callback
%%%

init([Host, Port]) ->
  process_flag(trap_exit, true), 
  {ok, _} = mochiweb_websocket:start(Host, Port, fun loop/3),
  game:start(),
  {ok, #pdata{ host = Host, port = Port }}.

handle_cast(stop, Data) ->
  {stop, normal, Data};

handle_cast(Msg, Data) ->
  ?LOG([{cast, Msg}]),
  {noreply, Data}.

handle_call({info, server}, _From, Data = #pdata{host = Host, port = Port}) ->
  {reply, {Host, Port}, Data};

handle_call({info, users}, _From, Data) ->
  {reply, 0, Data};

handle_call(Msg, _From, Data) ->
  ?LOG([{call, Msg}]),
  {noreply, Data}.

handle_info({'EXIT', Pid, Reason}, Data) ->
  ?LOG([{exit, Pid}, {reason, Reason}]),
  {noreply, Data};

handle_info(Msg, Data) ->
  ?LOG([{info, Msg}]),
  {noreply, Data}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

terminate(_, #pdata{port = Port}) ->
  mochiweb_websocket:stop(Port),
  game:stop().

%process_login(Client, Socket, Usr, Pass) ->
  %case login:login(Usr, Pass, self()) of
    %{error, Error} ->
      %ok = ?tcpsend(Socket, #bad{ cmd = ?CMD_LOGIN, error = Error}),
      %Client;
    %{ok, Player} ->
      %if
        %Client#client.player /= none ->
          %%% disconnect visitor
          %gen_server:cast(Client#client.player, 'DISCONNECT');
        %true ->
          %ok
      %end,
      %PID = gen_server:call(Player, 'ID'),
      %ok = ?tcpsend(Socket, #you_are{ player = PID }),
      %Client#client{ player = Player }
  %end.

%process_logout(Client, _Socket) ->
    %gen_server:cast(Client#client.player, #logout{}),
    %%% replace player process with a visitor
    %{ok, Visitor} = visitor:start(self()),
    %Client#client{ player = Visitor }.

%process_ping(Client, Socket, R) ->
    %ok = ?tcpsend(Socket, #pong{ orig_send_time = R#ping.send_time }),
    %Client.

%process_pong(Client, _Socket, R) ->
    %R1 = R#pong{ recv_time = now() },
    %gen_server:cast(Client#client.server, {'PONG', R1}),
    %Client.

%process_test_start_game(Client, Socket, R) ->
    %case gen_server:call(Client#client.server, 'TEST MODE') of
        %true ->
            %ok = ?tcpsend(Socket, start_test_game(R));
        %_ ->
            %ok
    %end,
    %Client.

%process_game_query(Client, Socket, Q) 
  %when is_record(Q, game_query) ->
    %find_games(Socket, 
               %Q#game_query.game_type, 
               %Q#game_query.limit_type,
               %Q#game_query.expected,
               %Q#game_query.joined,
               %Q#game_query.waiting),
    %Client.

%process_event(Client, _Socket, Event) ->
  %Client1 = if 
    %Client#client.player == none ->
      %%% start a proxy
      %{ok, Visitor} = visitor:start(self()),
      %Client#client{ player = Visitor };
    %true ->
      %Client
  %end,
  %gen_server:cast(Client1#client.player, Event),
  %Client1.

%parse_packet(Socket, tcp_closed, Client) ->
  %process_logout(Client, Socket);

%parse_packet(Socket, {packet, Packet}, Client) ->
%mochiweb_websocket:send(Socket, list_to_binary(pp:write(Packet))),
  %ok = ?tcpsend(Socket, Packet),
  %{loop_data, Client};

%parse_packet(Socket, {socket, Packet}, Client) ->
  %Data = (catch pp:read(Packet)),
  %case Data of
    %#ping{} -> opps;
    %#seat_query{} -> opps;
    %_ -> ok
  %end,

  %Client1 = case Data of 
    %{'EXIT', Error} ->
      %Client;
    %#login{ usr = Usr, pass = Pass} ->
      %process_login(Client, Socket, Usr, Pass);
    %#logout{} ->
      %process_logout(Client, Socket);
    %R = #ping{} ->
      %process_ping(Client, Socket, R);
    %R = #pong{} ->
      %process_pong(Client, Socket, R);
    %R = #start_game{ rigged_deck = [_|_] } ->
      %process_test_start_game(Client, Socket, R);
    %R when is_record(R, game_query) ->
      %process_game_query(Client, Socket, R);
    %Event ->
      %process_event(Client, Socket, Event)
  %end,
  %{loop_data, Client1};

%parse_packet(_Socket, Event, Client) ->
  %{loop_data, Client}.


%%%
%%% client
%%%

start() ->
  start("127.0.0.1", 8002).

start(Host, Port) ->
  {ok, _Pid} = gen_server:start({global, server}, [Host, Port], []).

stop() ->
  gen_server:cast({global, server}, stop).

%%% private 

%% handshake successful, init socket loop data
loop(S, handshake, ?UNDEF) -> #client{};

loop(S, {recv, Bin}, C= #client{}) when is_binary(Bin) ->
  loop(S, {recv, pp:read(Bin)}, C);

loop(S, {recv, #login{}}, #client{player = P}) when P =:= ?UNDEF ->
  #client{player = new_player_process}.
