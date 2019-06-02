%%%-------------------------------------------------------------------
%%% @author Jan Wilczek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2019 10:22 AM
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Jan Wilczek").
-behaviour(gen_server).

%% API
-export([start_link/1, init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3,
  getStationMean/2, getDailyMean/2, getCorrelation/3]).
-export([crash/0, test/0]).

start_link(InitialValue) ->     % server name           callback module
  gen_server:start_link({local, pollution_gen_server}, pollution_gen_server, InitialValue, []).

init(_) ->
  {ok, pollution:createMonitor()}.

%% User interface
start() ->
  start_link(placeholder).

stop() ->
  gen_server:cast(pollution_gen_server, stop).

%% Pollution functionality
addStation(StationName, StationCoordinates) ->
  gen_server:call(pollution_gen_server, {addStation, StationName, StationCoordinates}).

addValue(StationNameOrCoordinates, DateTime, Type, Value) ->
  gen_server:call(pollution_gen_server, {addValue, StationNameOrCoordinates, DateTime, Type, Value}).

removeValue(StationNameOrCoordinates, DateTime, Type) ->
  gen_server:call(pollution_gen_server, {removeValue, StationNameOrCoordinates, DateTime, Type}).

getOneValue(StationNameOrCoordinates, Date, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, StationNameOrCoordinates, Date, Type}).

getStationMean(StationNameOrCoordinates, Type) ->
  gen_server:call(pollution_gen_server, {getStationMean, StationNameOrCoordinates, Type}).

getDailyMean(Date, Type) ->
  gen_server:call(pollution_gen_server, {getDailyMean, Date, Type}).

getCorrelation(StationNameOrCoordinates, Type1, Type2) ->
  gen_server:call(pollution_gen_server, {getCorrelation, StationNameOrCoordinates, Type1, Type2}).

crash() ->
  gen_server:call(pollution_gen_server, {crash}).

%% Callback handling
handle_cast(stop, Monitor) ->
  {stop, normal, Monitor}.

handle_call({addStation, StationName, StationCoordinates}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Monitor, StationName, StationCoordinates),
  case NewMonitor of
    {error, _} -> {reply, NewMonitor, Monitor};
    _ -> {reply, ok, NewMonitor}
  end;
handle_call({addValue, StationNameOrCoordinates, DateTime, Type, Value}, _From, Monitor) ->
  NewMonitor = pollution:addValue(Monitor, StationNameOrCoordinates, DateTime, Type, Value),
  case NewMonitor of
    {error, _} -> {reply, NewMonitor, Monitor};
    _ -> {reply, ok, NewMonitor}
  end;
handle_call({getOneValue, StationNameOrCoordinates, Date, Type}, _From, Monitor) ->
  {reply, pollution:getOneValue(Monitor, StationNameOrCoordinates, Date, Type), Monitor};
handle_call({removeValue, StationNameOrCoordinates, DateTime, Type}, _From, Monitor) ->
  NewMonitor = pollution:removeValue(Monitor, StationNameOrCoordinates, DateTime, Type),
  case NewMonitor of
    {error, _} -> {reply, NewMonitor, Monitor};
    _ -> {reply, ok, NewMonitor}
  end;
handle_call({getStationMean, StationNameOrCoordinates, Type}, _From, Monitor) ->
  {reply, pollution:getStationMean(Monitor, StationNameOrCoordinates, Type), Monitor};
handle_call({getDailyMean, Date, Type}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Monitor, Date, Type), Monitor};
handle_call({getCorrelation, StationNameOrCoordinates, Type1, Type2}, _From, Monitor) ->
  {reply, pollution:getCorrelation(Monitor, StationNameOrCoordinates, Type1, Type2), Monitor};
handle_call({crash},  _From, _) ->
  1 / 0.

terminate(Reason, Value) ->
  io:format("Server: exit with value ~p~n.", [Value]),
  Reason.

%% Example usage: needs starting the server first.
test() ->
  pollution_gen_server:addStation("Aleje", {1,2}),
  pollution_gen_server:addValue("Kamionka", calendar:local_time(), "PM10", 1.0),
  pollution_gen_server:addStation("Kamionka", {4,5.0}),
  pollution_gen_server:addValue("Aleje", calendar:local_time(), "PM10", 4.0),
  pollution_gen_server:addValue("Aleje", calendar:local_time(), "PM10", 5.0),
  pollution_gen_server:addValue("Kamia", calendar:local_time(), "PM10", 1.0),
  pollution_gen_server:addValue({1,2}, calendar:local_time(), "PM10", 6.0),
  pollution_gen_server:addValue("Aleje", {{2019,4,18}, {16,21,00}}, "PM10", 2),
  pollution_gen_server:addValue("Aleje", {{2019,4,18}, {20,20,00}}, "PM10", 6),
  pollution_gen_server:addValue("Aleje", {{2019,4,18}, {20,20,00}}, "PM25", 10),
  pollution_gen_server:addValue("Aleje", {{2019,4,30}, {11,00,00}}, "PM25", 1),
  pollution_gen_server:addValue("Aleje", {{2019,4,30}, {11,00,00}}, "PM10", 3.5),
  pollution_gen_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_gen_server:removeValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_gen_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_gen_server:getStationMean("Aleje", "PM10"),
  pollution_gen_server:getStationMean("Kamionka", "PM10"),
  pollution_gen_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_gen_server:getDailyMean({2019, 4, 18}, "PM10").
