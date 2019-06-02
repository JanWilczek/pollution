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
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3]).

start_link(InitialValue) ->     % server name           callback module
  gen_server:start_link({local, pollution_gen_server}, pollution_gen_server, InitialValue, []).

init(_) ->
  {ok, pollution:createMonitor()}.

%% User interface
start() ->
  start_link(placeholder).

stop() ->
  gen_server:cast(pollution_gen_server, stop).


addStation(StationName, StationCoordinates) ->
  gen_server:call(pollution_gen_server, {addStation, StationName, StationCoordinates}).

addValue(StationNameOrCoordinates, DateTime, Type, Value) ->
  gen_server:call(pollution_gen_server, {addValue, StationNameOrCoordinates, DateTime, Type, Value}).

getOneValue(StationNameOrCoordinates, Date, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, StationNameOrCoordinates, Date, Type}).

removeValue(StationNameOrCoordinates, DateTime, Type) ->
  gen_server:call({removeValue, StationNameOrCoordinates, DateTime, Type}).

%%getOneValue(StationNameOrCoordinates, DateTime, Type) ->
%%  call({getOneValue, StationNameOrCoordinates, DateTime, Type}).
%%
%%getStationMean(StationNameOrCoordinates, Type) ->
%%  call({getStationMean, StationNameOrCoordinates, Type}).
%%
%%getDailyMean(Date, Type) ->
%%  call({getDailyMean, Date, Type}).
%%
%%getCorrelation(StationNameOrCoordinates, Type1, Type2) ->
%%  call({getCorrelation, StationNameOrCoordinates, Type1, Type2}).
%%
%%crash() ->
%%  server_monitor ! {request, self(), crash}.

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
  end.

terminate(Reason, Value) ->
  io:format("Server: exit with value ~p~n.", [Value]),
  Reason.