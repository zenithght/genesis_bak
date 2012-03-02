-module(sim_client).
-export([start/1, loop/1, box/1, head/1]).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(pdata, {
    box = [],
    host = ?UNDEF
  }).

%%%
%%% client
%%%

start(Id) when is_atom(Id) ->
  PID = spawn(?MODULE, loop, [fun client:loop/2]),
  true = register(Id, PID),
  PID.

head(Id) ->
  Id ! {head, self()},
  receive 
    R when is_tuple(R) -> R;
    nil -> nil
  after
    1000 -> exit(request_timeout)
  end.

box(Id) ->
  Id ! {box, self()},
  receive 
    Box when is_list(Box) -> Box
  after
    1000 -> exit(request_timeout)
  end.

%%%
%%% callback
%%%

loop(Fun) ->
  loop(Fun, ?UNDEF, #pdata{}).

%%%
%%% private
%%%

loop(Fun, ?UNDEF, Data = #pdata{}) ->
  LoopData = Fun(connected, ?UNDEF),
  loop(Fun, LoopData, Data);

loop(Fun, LoopData, Data = #pdata{box = Box}) ->
  receive
    %% clien module callback close connection.
    close ->
      exit(normal);
    %% clien module callback send bianry to remote client.
    {send, Bin} when is_binary(Bin) ->
      NB = [pp:read(Bin) | Box], %% insert new message to box
      loop(Fun, LoopData, Data#pdata{box = NB});
    %% send protocol record to clinet module.
    {send, R} when is_tuple(R) ->
      ND = Fun({recv, list_to_binary(pp:write(R))}, LoopData),
      loop(Fun, ND, Data); %% sim socket binary data
    %% host process get message box head one.
    {head, From} when is_pid(From) ->
      case Box of
        [H|T] ->
          From ! H,
          loop(Fun, LoopData, Data#pdata{box = T});
        [] ->
          From ! nil,
          loop(Fun, LoopData, Data#pdata{box = []})
      end;
    {box, From} when is_pid(From) ->
      From ! Box,
      loop(Fun, LoopData, Data#pdata{box = []});
    Msg ->
      ND = Fun({msg, Msg}, LoopData),
      loop(Fun, ND, Data)
  end.

%%%
%%% unit test
%%%

start_test() ->
  ?assert(is_pid(sim_client:start(test))).
