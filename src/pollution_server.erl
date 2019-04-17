%%%-------------------------------------------------------------------
%%% @author Jan Wilczek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2019 10:23 AM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Jan Wilczek").

%% API
-export([start/0, stop/0, init/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2]).

start() ->
  Pid = spawn_link(?MODULE, init, []),
  register(server_monitor, Pid).

init() ->
  InitialState = pollution:createMonitor(),
  serverLoop(InitialState).

stop() ->
  server_monitor ! stop.

serverLoop(Monitor) ->
  receive
    {request, Pid, {add_station, StationName, StationCoordinates}} ->
      NewMonitor = pollution:addStation(Monitor, StationName, StationCoordinates),
      case NewMonitor of
        {error, _} -> Pid ! {response, NewMonitor},
          serverLoop(Monitor);
        _ -> Pid ! {response, ok},
          serverLoop(NewMonitor)
      end;

    {request, Pid, {addValue, StationNameOrCoordinates, Date, Type, Value}} ->
      NewMonitor = pollution:addValue(Monitor, StationNameOrCoordinates, Date, Type, Value),
      case NewMonitor of
        {error, _} ->
          Pid ! {response, NewMonitor},
          serverLoop(Monitor);
        _ -> Pid ! {response, ok},
          serverLoop(NewMonitor)
      end;

    {request, Pid, stop} ->
      Pid ! {response, ok}
  after
    100000 -> self() ! stop
  end.

call(Message) ->
  server_monitor ! {request, self(), Message},
  receive
    {response, Reply} -> Reply
  end.

addStation(StationName, StationCoordinates) ->
  call({add_station, StationName, StationCoordinates}).

addValue(StationNameOrCoordinates, DateTime, Type, Value) ->
  call({addValue, StationNameOrCoordinates, DateTime, Type, Value}).

removeValue(StationNameOrCoordinates, DateTime, Type) ->
  call({removeValue, StationNameOrCoordinates, DateTime, Type}).

getOneValue(StationNameOrCoordinates, DateTime, Type) ->
  call({getOneValue, StationNameOrCoordinates, DateTime, Type}).

getStationMean(StationNameOrCoordinates, Type) ->
  call({getStationMean, StationNameOrCoordinates, Type}).

getDailyMean(Date, Type) ->
  call({getDailyMean, Date, Type}).

%getCorrelation


