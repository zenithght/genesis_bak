-module(player).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1, stop/2, update_photo/2]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("pp.hrl").
-include("schema.hrl").

-define(PLAYER(Identity), {global, {player, Identity}}).
-define(LOOKUP_PLAYER(Identity), global:whereis_name({player, Identity})).

-record(pdata, {
    pid,
    self,
    socket = ?UNDEF,
    playing = ?UNDEF,
    watching = ?UNDEF,
    nick = ?UNDEF,
    photo = ?UNDEF,

    zombie 
  }).

init([#tab_player_info{pid = PID, nick = Nick, photo = Photo}]) ->
  process_flag(trap_exit, true),
  ok = create_runtime(PID, self()),
  {ok, #pdata{ pid = PID, self = self(), nick = Nick, photo = Photo}}.

handle_cast({protocol, #watch{}}, Data = #pdata{watching = W}) when W /= ?UNDEF ->
  {noreply, Data};

handle_cast({protocol, R = #watch{game = G}}, Data) ->
  game:watch(G, self()),
  {noreply, Data#pdata{ watching = G}};

handle_cast({protocol, R = #unwatch{game = G}}, Data = #pdata{watching = W}) when W /= G->
  {noreply, Data};

handle_cast({protocol, #unwatch{game = G}}, Data) ->
  game:unwatch(G, player = self()),
  {noreply, Data#pdata{ watching = ?UNDEF}};

handle_cast(P = {protocol, #join{ game = G}}, Data = #pdata{watching = W}) when W =:= ?UNDEF ->
  game:watch(G, self()),
  handle_cast(P, Data#pdata{watching = G});

handle_cast({protocol, R = #join { game = G}}, Data = #pdata{watching = W}) when W /= G ->
  {noreply, Data};

handle_cast({protocol, R = #join { seat = Seat, game = Game}}, Data = #pdata{}) ->
  game:join(Game, Seat, self()),
  {noreply, Data};

handle_cast({protocol, #leave{game = G}}, Data = #pdata{playing = P}) when G /= P ->
  {noreply, Data};

handle_cast({protocol, #leave{game = G}}, Data) ->
  game:leave(G, self()),
  {noreply, Data};

handle_cast({protocol, #logout{}}, Data = #pdata{playing = P}) when P =:= ?UNDEF  ->
  handle_cast(stop, Data);

handle_cast({protocol, #logout{}}, Data = #pdata{playing = P}) ->
  game:leave(P, self()),
  {noreply, Data};

handle_cast({protocol, #player_query{ player = P }}, Data = #pdata{pid = PID}) when P =:= PID ->
  R = #player_info{
    player = PID,
    nick = Data#pdata.nick,
    photo = Data#pdata.photo
  },
  handle_cast({notify, R}, Data);

handle_cast({protocol, #player_query{ player = PID }}, Data) ->
  case mnesia:drity_read(tab_player_info, PID) of
    [Info] ->
      R = #player_info{
        player = Info#tab_player_info.pid,
        nick = Info#tab_player_info.nick,
        photo = Info#tab_player_info.photo
      },
      handle_cast({notify, R}, Data);
    _ ->
      {noreply, Data}
  end;

handle_cast({protocol, #seat_query{ game = G }}, Data = #pdata{watching = W, playing = P}) when W =:= ?UNDEF, P =:= ?UNDEF ->
  L = game:seat_query(G),
  F = fun(R = #seat_state{}) ->
      forward_to_client(R, Data)
  end,
  lists:foreach(F, L),
  {noreply, Data};
      
handle_cast({protocol, #balance_query{}}, Data) ->
  R = #balance{ amount = 0, inplay = 0 },
  handle_cast({notify, R}, Data);

handle_cast({protocol, R}, Data = #pdata{playing = Playing}) ->
  Game = element(2, R),
  case Game of
    Playing ->
      game:action(Game, self(), R),
      {noreply, Data};
    _ ->
      {noreply, Data}
  end;

handle_cast({notify, R = #notify_join{proc = G, player = P}}, Data = #pdata{pid = PID}) when P =:= PID ->
  forward_to_client(R, Data),
  {noreply, Data#pdata{ playing = G }};

handle_cast({notify, R = #notify_leave{player = P}}, Data = #pdata{pid = PID}) when P =:= PID ->
  forward_to_client(R, Data),
  {noreply, Data#pdata{ playing = ?UNDEF }};

handle_cast({notify, R}, Data) ->
  forward_to_client(R, Data),
  {noreply, Data};

handle_cast(stop, Data) ->
  {stop, normal, Data};

handle_cast({stop, Reason}, Data) ->
  {stop, Reason, Data};

handle_cast(R, Data) ->
  error_logger:info_report([{module, ?MODULE}, {process, self()}, {unknown_cast, R}]),
  {noreply, Data}.

handle_call({socket, Socket}, _From, Data) ->
  {reply, ok, Data#pdata{ socket = Socket }};

handle_call(kill, _From, Data) ->
  {stop, normal, ok, Data};

handle_call(pdata, _From, Data) ->
  {reply, Data, Data};

handle_call(R, From, Data) ->
  error_logger:info_report([{module, ?MODULE}, {process, self()}, {unknown_call, R}, {from, From}]),
  {noreply, Data}.

terminate(_Reason, Data) ->
  ok = db:delete(tab_player, Data#pdata.pid).

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
  ?LOG([{player_unkonw_info, {info, Info}}]),
  {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
  {ok, Data}.

%%%
%%% clinet function
%%% 

start(Identity) when is_binary(Identity) ->
  start(binary_to_list(Identity));
start(Identity) when is_list(Identity) ->
    case mnesia:dirty_index_read(tab_player_info, Identity, identity) of
        [PlayerInfo] ->
          gen_server:start({global, {player, Identity}}, player, [PlayerInfo], []);
        _ ->
          {error, not_found_player}
    end.

stop(Identity) when is_list(Identity) ->
  gen_server:cast(?PLAYER(Identity), stop).

stop(Identity, Reason) when is_list(Identity) ->
    gen_server:cast(?PLAYER(Identity), {stop, Reason}).

update_photo(Identity, Photo) when is_list(Identity), is_binary(Photo) ->
  gen_server:cast(?PLAYER(Identity), {update_photo, Photo}).

notify(Identity, R) ->
  gen_server:cast(?PLAYER(Identity), {notify, R}).

cast(Identity, R) ->
  gen_server:cast(?PLAYER(Identity), {protocol, R}).

%%%
%%% private function
%%%

create_runtime(PID, Process) when is_number(PID), is_pid(Process) ->
  mnesia:dirty_write(#tab_player{ pid = PID, process = Process }).

forward_to_client(_, Data = #pdata{socket = S}) when S =:= ?UNDEF -> ok;
forward_to_client(R, Data = #pdata{socket = S}) -> S ! {packet, R}.

%%%
%%% unit test
%%%

start_test() ->
  setup(),
  {ok, _Pid} = start("player_1"),
  ?assertEqual(true, erlang:is_process_alive(?LOOKUP_PLAYER("player_1"))),
  Pdata = pdata("player_1"),
  ?assertEqual("player1", Pdata#pdata.nick),
  ?assertEqual("default1", Pdata#pdata.photo),
  [Xref] = mnesia:dirty_read(tab_player, Pdata#pdata.pid),
  ?assertEqual(Pdata#pdata.self, Xref#tab_player.process),
  ?assertEqual(undefined, Xref#tab_player.socket).
  
setup() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data(),

  Players = [
    #tab_player_info {
      pid = counter:bump(player),
      identity = "player_1",
      nick = "player1",
      photo = "default1",
      agent = "root"
    }, #tab_player_info {
      pid = counter:bump(player),
      identity = "player_2",
      nick = "player2",
      agent = "root"
    }, #tab_player_info {
      pid = counter:bump(player),
      identity = "player_3",
      nick = "player3",
      agent = "agent_1"
    }, #tab_player_info {
      pid = counter:bump(player),
      identity = "player_4",
      nick = "player4",
      agent = "agent_1_1"
    }
  ],

  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Players),
  
  Fun = fun(#tab_player_info{identity = Identity}, _Acc) ->
      case ?LOOKUP_PLAYER(Identity) of
        Pid when is_pid(Pid) -> 
          ok = gen_server:call(Pid, kill);
        _ -> ok
      end
  end,
  lists:foldl(Fun, nil, Players).

pdata(Identity) ->
  gen_server:call(?PLAYER(Identity), pdata).

% seat_state);
% bet_req);
% player_info);
% photo_info);
% game_stage);
% notify_start_game);
% notify_end_game);
% notify_cancel_game);
% notify_join);
% notify_unwatch);
% notify_draw);
% notify_actor);
% notify_private);
% notify_shared);
% notify_leave);
% notify_raise);
% notify_blind);
% notify_win);
% notify_hand);
% show_cards);
% notify_button);
% notify_sb);
% notify_game_detail);
% limit);
% notify_seat_detail);
% notify_bb) ->

