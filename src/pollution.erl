%%%-------------------------------------------------------------------
%%% @author Jan Wilczek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2019 12:22 PM
%%%-------------------------------------------------------------------
-module(pollution).
-author("Jan Wilczek").
-include("pollution.hrl").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getCorrelation/4]).


createMonitor() ->
  #monitor{ stations=#{} }.

addStation(#monitor{ stations = Stations}, StationName, StationCoordinates) ->
  addStationChecked(maps:is_key(StationName, Stations) orelse
    lists:any(fun (#station{ coordinates = Coordinates, measurements = _}) -> StationCoordinates == Coordinates end, maps:values(Stations)),
    #monitor{ stations = Stations }, StationName, StationCoordinates).

addStationChecked(StationExists, #monitor{ stations = Stations}, StationName, StationCoordinates) ->
  case StationExists of
    true  -> {error, "Failed to add station: station with the given name or coordinates already exists."};
    false ->  #monitor{ stations = maps:put(StationName, #station{ coordinates = StationCoordinates, measurements = []}, Stations)}
  end.

addValue(#monitor { stations = Stations }, StationName, Date, MeasurementType, MeasurementValue) when is_list(StationName) and is_list(MeasurementType) and is_number(MeasurementValue) ->
  addValueCheckedStation(#monitor {stations =  Stations}, maps:is_key(StationName, Stations), StationName, Date, MeasurementType, MeasurementValue);
addValue(#monitor { stations = Stations }, Coordinates, Date, MeasurementType, MeasurementValue) when is_tuple(Coordinates) and is_list(MeasurementType) and is_number(MeasurementValue) ->
  addValueCheckedStation(#monitor {stations =  Stations},
    lists:any(fun (#station{ coordinates = Coords, measurements = _ }) -> Coordinates == Coords end, maps:values(Stations)),
    stationNameFromCoordinates(Stations, Coordinates), Date, MeasurementType, MeasurementValue).

addValueCheckedStation(#monitor {stations =  Stations}, StationExists, StationName, Date, MeasurementType, MeasurementValue) ->
  case StationExists of
    true  ->  #station{ coordinates = _, measurements = Measurements} = maps:get(StationName, Stations),
              addValueCheckedEntry(#monitor {stations =  Stations},
                lists:any(fun (#measurement{ date = SearchedDate, type = SearchedType, value = _ })-> SearchedDate == Date andalso SearchedType == MeasurementType end, Measurements),
                StationName, Date, MeasurementType, MeasurementValue);
    false -> {error, "Failed to add measured value: no station under given name or coordinates."}
  end.

stationNameFromCoordinates(Stations, Coordinates) when is_map(Stations) and is_tuple(Coordinates) ->
  CoordinatesSearchMap = maps:filter(fun (_, #station{coordinates = Coords, measurements = _}) -> Coords == Coordinates end, Stations),
  case maps:size(CoordinatesSearchMap) of
    1   ->  lists:last(maps:keys(CoordinatesSearchMap));
    _   -> {error, "The monitor is corrupted: two stations under the same coordinates."}
  end.

addValueCheckedEntry(#monitor {stations =  Stations}, EntryExists, StationName, Date, MeasurementType, MeasurementValue) ->
  case EntryExists of
    false -> #station{ coordinates = Coordinates, measurements = Measurements} = maps:get(StationName, Stations),
              #monitor{ stations = Stations#{
                StationName := #station{ coordinates = Coordinates, measurements = Measurements ++ [#measurement{date = Date, type = MeasurementType, value = MeasurementValue}] }}};
    true  -> {error, "Failed to add measured value: the measurement for this station, date, time and type has already been adde d."}
  end.

removeValue(#monitor {stations = Stations}, StationCoordinates, Date, MeasurementType) when is_map(Stations) and is_tuple(StationCoordinates) and is_tuple(Date) and is_list(MeasurementType) ->
  removeValue(#monitor {stations = Stations}, stationNameFromCoordinates(Stations, StationCoordinates), Date, MeasurementType);
removeValue(#monitor { stations = Stations}, StationName, Date, MeasurementType) when is_map(Stations) and is_list(StationName) and is_tuple(Date) and is_list(MeasurementType) ->
  Station = maps:get(StationName, Stations),
  #station{ coordinates = Coordinates, measurements = Measurements } = Station,
  UpdatedStations = maps:put( StationName, #station{coordinates = Coordinates, measurements =
      lists:filter(fun (#measurement{ date = D, type = T, value = _}) -> D /= Date orelse T /= MeasurementType end, Measurements)}, Stations),
  #monitor { stations = UpdatedStations }.

getOneValue(#monitor {stations = Stations}, StationCoordinates, Date, MeasurementType) when is_map(Stations) and is_tuple(StationCoordinates) and is_tuple(Date) and is_list(MeasurementType) ->
  getOneValue(#monitor {stations = Stations}, stationNameFromCoordinates(Stations, StationCoordinates), Date, MeasurementType);
getOneValue(#monitor {stations = Stations}, StationName, Date, MeasurementType) when is_map(Stations) and is_list(StationName) and is_tuple(Date) and is_list(MeasurementType) ->
  #station{ coordinates = _, measurements = Measurements} = maps:get(StationName, Stations),
  Result = lists:search(fun (#measurement{ date = D, type = T, value = _ }) -> D == Date andalso T == MeasurementType end, Measurements),
  case Result of
    {value, Value} -> Value;
    _ -> {error, "No measurements corresponding to the given criteria."}
  end.

getStationMean(#monitor {stations = Stations}, StationCoordinates, MeasurementType) when is_map(Stations) andalso is_tuple(StationCoordinates) and is_list(MeasurementType) ->
  getStationMean(#monitor {stations = Stations}, stationNameFromCoordinates(Stations, StationCoordinates), MeasurementType);
getStationMean(#monitor {stations = Stations}, StationName, MeasurementType) when is_map(Stations) andalso is_list(StationName) and is_list(MeasurementType) ->
  Measurements = getStationMeasurements(Stations, StationName),
  case length(Measurements) of
    0 -> {error, "No measurements at that station"};
    _ -> getMeanFromMeasurements(Measurements, MeasurementType)
  end.

getMeanFromMeasurements(Measurements, Type) when is_list(Measurements) and is_list(Type) ->
  Values = getValuesOfType(Measurements, Type),
  case length(Values) of
    0 -> {error, "No measurements of given type at that station"};
    _ -> lists:sum(Values) / length(Values)
  end.

getDailyMean(#monitor {stations = Stations}, Date, MeasurementType) when is_map(Stations) andalso is_tuple(Date) and is_list(MeasurementType) ->
  AllMeasurements = lists:append(
    maps:values(
      maps:map(fun (_, #station{ coordinates = _ , measurements = Measurements}) -> Measurements end, Stations))),
  DateTypeMeasurementValues = lists:map(fun (#measurement{ date = _, type = _, value = V}) -> V end,
    lists:filter(fun (#measurement{ date = {D, _}, type = T, value = _ }) ->  T == MeasurementType  andalso D == Date end, AllMeasurements)),
  case length(DateTypeMeasurementValues) of
    0 -> {error, "No measurements that fit the given date and type."};
    _ -> lists:sum(DateTypeMeasurementValues) / length(DateTypeMeasurementValues)
  end.

% For measurements of different types, that where taken at the same time calculate the difference,
% raise to second power, sum, divide by number of measurements minus one and calculate the square root.
getCorrelation(#monitor {stations = Stations}, StationCoordinates, MeasurementType1, MeasurementType2) when is_map(Stations) and is_tuple(StationCoordinates) and is_list(MeasurementType1) and is_list(MeasurementType2) ->
  getCorrelation(#monitor {stations = Stations}, stationNameFromCoordinates(Stations, StationCoordinates), MeasurementType1, MeasurementType2);
getCorrelation(#monitor {stations = Stations}, StationName, MeasurementType1, MeasurementType2) when is_map(Stations) and is_list(StationName) and is_list(MeasurementType1) and is_list(MeasurementType2) ->
  Measurements = getStationMeasurements(Stations, StationName),
  MeasurementsType1 = getMeasurementsOfType(Measurements, MeasurementType1),
  MeasurementsType2 = getMeasurementsOfType(Measurements, MeasurementType2),
  getCorrelationFromMeasurements(0, 0, MeasurementsType1, MeasurementsType2).

getCorrelationFromMeasurements(DiffSquaredSum, Summed, [], _) ->
  case Summed of
      0 -> 0;
      1 -> math:sqrt(DiffSquaredSum);
      _ -> math:sqrt(DiffSquaredSum / (Summed - 1))
  end;
getCorrelationFromMeasurements(DiffSquaredSum, Summed, _, []) ->
  getCorrelationFromMeasurements(DiffSquaredSum, Summed, [], []);
getCorrelationFromMeasurements(DiffSquaredSum, Summed, [ #measurement{date = Date, type = _, value = Value} | T ], Measurements2) ->
  case lists:search(fun (#measurement{ date = D, type = _, value = _}) -> Date == D end, Measurements2) of
    false -> getCorrelationFromMeasurements(DiffSquaredSum, Summed, T, Measurements2);
    {value, #measurement{ date = _, type = _, value = V}} -> getCorrelationFromMeasurements(DiffSquaredSum + math:pow(Value - V, 2), Summed + 1, T, Measurements2)
  end.

getValuesOfType(Measurements, Type) when is_list(Measurements) and is_list(Type) ->
  lists:map(fun (#measurement{date = _, type = _, value = V}) -> V end,
    getMeasurementsOfType(Measurements, Type)).

getMeasurementsOfType(Measurements, Type) when is_list(Measurements) and is_list(Type) ->
  lists:filter(fun (#measurement{ date = _, type = T, value = _ }) -> Type == T end, Measurements).

getStationMeasurements(Stations, StationName) when is_map(Stations) and is_list(StationName) ->
  #station{ coordinates = _, measurements = Measurements} = maps:get(StationName, Stations),
  Measurements.