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
-export([start/0, stop/0, init/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getCorrelation/3, test/0, crash/0]).

start() ->
  Pid = spawn_link(?MODULE, init, []),
  register(server_monitor, Pid).

init() ->
  InitialState = pollution:createMonitor(),
  serverLoop(InitialState).

stop() ->
  call(stop).

serverLoop(Monitor) ->
  receive
    {request, Pid, {addStation, StationName, StationCoordinates}} ->
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

    {request, Pid, {removeValue, StationNameOrCoordinates, Date, Type}} ->
      NewMonitor = pollution:removeValue(Monitor, StationNameOrCoordinates, Date, Type),
      case NewMonitor of
        {error, _} ->
          Pid ! {response, NewMonitor},
          serverLoop(Monitor);
        _ -> Pid ! {response, ok},
          serverLoop(NewMonitor)
      end;

    {request, Pid, {getOneValue, StationNameOrCoordinates, Date, Type}} ->
      Pid ! {response, pollution:getOneValue(Monitor, StationNameOrCoordinates, Date, Type)},
      serverLoop(Monitor);

    {request, Pid, {getStationMean, StationNameOrCoordinates, Type}} ->
      Pid ! {response, pollution:getStationMean(Monitor, StationNameOrCoordinates, Type)},
      serverLoop(Monitor);

    {request, Pid, {getDailyMean, Date, Type}} ->
      Pid ! {response, pollution:getDailyMean(Monitor, Date, Type)},
      serverLoop(Monitor);

    {request, Pid, {getCorrelation, StationNameOrCoordinates, Type1, Type2}} ->
      Pid ! {response, pollution:getCorrelation(Monitor, StationNameOrCoordinates, Type1, Type2)},
      serverLoop(Monitor);

    {request, Pid, crash} ->
      % Pid ! {response, 1 / 0};
      1 / 0;

    {request, Pid, stop} ->
      Pid ! {response, ok}
  end.

call(Message) ->
  server_monitor ! {request, self(), Message},
  receive
    {response, Reply} -> Reply
  end.

addStation(StationName, StationCoordinates) ->
  call({addStation, StationName, StationCoordinates}).

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

getCorrelation(StationNameOrCoordinates, Type1, Type2) ->
  call({getCorrelation, StationNameOrCoordinates, Type1, Type2}).

crash() ->
  server_monitor ! {request, self(), crash}.

test() ->
  pollution_server:start(),
  pollution_server:addStation("Aleje", {1,2}),
  pollution_server:addValue("Kamionka", calendar:local_time(), "PM10", 1.0),
  pollution_server:addStation("Kamionka", {4,5.0}),
  pollution_server:addValue("Aleje", calendar:local_time(), "PM10", 4.0),
  pollution_server:addValue("Aleje", calendar:local_time(), "PM10", 5.0),
  pollution_server:addValue("Kamia", calendar:local_time(), "PM10", 1.0),
  pollution_server:addValue({1,2}, calendar:local_time(), "PM10", 6.0),
  pollution_server:addValue("Aleje", {{2019,4,18}, {16,21,00}}, "PM10", 2),
  pollution_server:addValue("Aleje", {{2019,4,18}, {20,20,00}}, "PM10", 6),
  pollution_server:addValue("Aleje", {{2019,4,18}, {20,20,00}}, "PM25", 10),
  pollution_server:addValue("Aleje", {{2019,4,30}, {11,00,00}}, "PM25", 1),
  pollution_server:addValue("Aleje", {{2019,4,30}, {11,00,00}}, "PM10", 3.5),
  pollution_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_server:removeValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_server:getStationMean("Aleje", "PM10"),
  pollution_server:getStationMean("Kamionka", "PM10"),
  pollution_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_server:getDailyMean({2019, 4, 18}, "PM10").

