-module(player).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1, stop/2, notify/2, cast/2, auth/2, logout/1]).

-export([client/1, info/1, balance/1]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("protocol.hrl").
-include("schema.hrl").

-define(PLAYER(Identity), {global, {player, Identity}}).
-define(LOOKUP_PLAYER(Identity), global:whereis_name({player, Identity})).

-record(pdata, {
    pid,
    self,
    client = ?UNDEF,
    playing = ?UNDEF,
    watching = ?UNDEF,
    nick = ?UNDEF,
    photo = ?UNDEF,
    inplay = 0,
    record,
    zombie 
  }).

init([R = #tab_player_info{pid = PID, nick = Nick, photo = Photo}]) ->
  process_flag(trap_exit, true),
  ok = create_runtime(PID, self()),
  {ok, #pdata{ pid = PID, self = self(), nick = list_to_binary(Nick), photo = list_to_binary(Photo), record = R}}.

handle_cast(#watch{}, Data = #pdata{watching = W}) when W /= ?UNDEF ->
  {noreply, Data};

handle_cast(#watch{game = G}, Data) ->
  game:watch(G, self()),
  {noreply, Data#pdata{ watching = G}};

handle_cast(#unwatch{game = G}, Data = #pdata{watching = W}) when W /= G->
  {noreply, Data};

handle_cast(#unwatch{game = G}, Data) ->
  game:unwatch(G, player = self()),
  {noreply, Data#pdata{ watching = ?UNDEF}};

handle_cast(R = #join{ game = G}, Data = #pdata{watching = W}) when W =:= ?UNDEF ->
  game:watch(G, self()),
  handle_cast(R, Data#pdata{watching = G});

handle_cast(#join { game = G}, Data = #pdata{watching = W}) when W /= G ->
  {noreply, Data};

handle_cast(#join { seat = Seat, game = Game}, Data = #pdata{}) ->
  game:join(Game, Seat, self()),
  {noreply, Data};

handle_cast(#leave{game = G}, Data = #pdata{playing = P}) when G /= P ->
  {noreply, Data};

handle_cast(#leave{game = G}, Data) ->
  game:leave(G, self()),
  {noreply, Data};

handle_cast(#player_query{}, Data = #pdata{}) ->
  R = #player_info{
    player = Data#pdata.pid,
    inplay = Data#pdata.inplay,
    nick = Data#pdata.nick,
    photo = Data#pdata.photo
  },
  handle_cast({notify, R}, Data);

handle_cast(#seat_query{ game = G }, Data = #pdata{watching = W, playing = P}) when W =:= ?UNDEF, P =:= ?UNDEF ->
  L = game:seat_query(G),
  F = fun(R = #seat_state{}) ->
      forward_to_client(R, Data)
  end,
  lists:foreach(F, L),
  {noreply, Data};
      
handle_cast(#balance_query{}, Data) ->
  R = #balance{ amount = 0, inplay = 0 },
  handle_cast({notify, R}, Data);

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

handle_cast(R, Data = #pdata{playing = Playing}) ->
  Game = element(2, R),
  case Game of
    Playing ->
      game:action(Game, self(), R),
      {noreply, Data};
    _ ->
      {noreply, Data}
  end;

handle_cast(R, Data) ->
  ?LOG([{unknown_cast, R}]),
  {noreply, Data}.

handle_call({client, Client}, _From, Data) when is_pid(Client) ->
  {reply, Client, Data#pdata{ client = Client}};

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

handle_info(_Info, Data) ->
  {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
  {ok, Data}.

%%%
%%% clinet function
%%% 

start(Identity) when is_list(Identity) ->
  [R] = mnesia:dirty_index_read(tab_player_info, Identity, identity),
  start(R);
  
start(R = #tab_player_info{identity = Identity}) ->
  gen_server:start(?PLAYER(Identity), player, [R], []).

stop({kill, Identity}) when is_list(Identity) ->
  catch ok = gen_server:call(?PLAYER(Identity), kill);

stop(Identity) when is_list(Identity) ->
  gen_server:cast(?PLAYER(Identity), stop).

stop(Identity, Reason) when is_list(Identity) ->
  gen_server:cast(?PLAYER(Identity), {stop, Reason}).

notify(Identity, R) when is_list(Identity) ->
  notify(?LOOKUP_PLAYER(Identity), R);
notify(Player, R) when is_pid(Player) ->
  gen_server:cast(Player, {notify, R}).

cast(Player, R) when is_pid(Player) ->
  gen_server:cast(Player, R).

auth(Identity, Password) when is_list(Identity), is_list(Password) ->
  ok = mnesia:wait_for_tables([tab_player_info], ?WAIT_TABLE),
  case mnesia:dirty_index_read(tab_player_info, Identity, identity) of
    [Info] ->
      auth(Info, Password);
    _ ->
      {ok, unauth}
  end;

auth(Info = #tab_player_info{password = Pwd}, Password) when is_list(Password) ->
  %% password processed by phash, result is a integer
  case erlang:phash2(Password, 1 bsl 32) =:= Pwd of
    true -> auth(Info, player_disable);
    _ -> {ok, unauth}
  end;

auth(Info = #tab_player_info{disabled = Disabled}, player_disable) ->
  case Disabled of
    false -> auth(Info, agent_disable);
    _ -> {ok, player_disable}
  end;

auth(Info = #tab_player_info{agent = _Agent}, agent_disable) ->
  %% TODO: auth agnet is alive and disable
  {ok, pass, Info}.

logout(Player) when is_pid(Player) ->
  gen_server:call(Player, logout).

info(Player) when is_pid(Player) ->
  gen_server:cast(Player, #player_query{}).

balance(Player) when is_pid(Player) ->
  gen_server:cast(Player, #balance_query{}).

client(Player) when is_pid(Player) ->
  Client = self(),
  Client = gen_server:call(Player, {client, Client}).

%%%
%%% private
%%%

create_runtime(PID, Process) when is_number(PID), is_pid(Process) ->
  mnesia:dirty_write(#tab_player{ pid = PID, process = Process }).

forward_to_client(_, #pdata{client = Client}) when Client =:= ?UNDEF -> exit(undefined_client);
forward_to_client(R, #pdata{client = Client}) -> client:send(Client, R).

%%%
%%% unit test
%%%

-define(DEF_PWD, "def_pwd").
-define(DEF_HASH_PWD, erlang:phash2(?DEF_PWD, 1 bsl 32)).
  
start_all_test() ->
  setup(),
  start("player_1"),
  ?assertEqual(true, erlang:is_process_alive(?LOOKUP_PLAYER("player_1"))),
  Pdata = pdata("player_1"),
  ?assertEqual(<<"player1">>, Pdata#pdata.nick),
  ?assertEqual(<<"default1">>, Pdata#pdata.photo),
  [Xref] = mnesia:dirty_read(tab_player, Pdata#pdata.pid),
  ?assertEqual(Pdata#pdata.self, Xref#tab_player.process),
  ?assertEqual(undefined, Xref#tab_player.socket).

start_test() ->
  setup(),
  {ok, _Pid} = start("player_1"),
  ?assertEqual(true, erlang:is_process_alive(?LOOKUP_PLAYER("player_1"))),
  Pdata = pdata("player_1"),
  ?assertEqual(<<"player1">>, Pdata#pdata.nick),
  ?assertEqual(<<"default1">>, Pdata#pdata.photo),
  [Xref] = mnesia:dirty_read(tab_player, Pdata#pdata.pid),
  ?assertEqual(Pdata#pdata.self, Xref#tab_player.process),
  ?assertEqual(undefined, Xref#tab_player.socket).

auth_test() ->
  setup(),
  [R] = mnesia:dirty_index_read(tab_player_info, "player_1", identity),
  ?assertEqual({ok, pass, R}, player:auth("player_1", ?DEF_PWD)),
  ?assertEqual({ok, unauth}, player:auth("player_nil", ?DEF_PWD)),
  ?assertEqual({ok, unauth}, player:auth("player_1", "bad_pwd")),

  Info = #tab_player_info{identity = "player_1", password = ?DEF_HASH_PWD, disabled = false},
  ?assertEqual({ok, pass, Info}, player:auth(Info, ?DEF_PWD)),
  ?assertEqual({ok, unauth}, player:auth(Info, "bad_pwd")),
  ?assertEqual({ok, player_disable}, player:auth(Info#tab_player_info{disabled = true}, ?DEF_PWD)).

setup() ->
  schema:uninstall(),
  schema:install(),
  schema:load_default_data(),

  Players = [
    #tab_player_info {
      pid = counter:bump(player),
      identity = "player_1",
      password = ?DEF_HASH_PWD,
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
