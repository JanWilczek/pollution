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

%% API
-export([createMonitor/0, addStation/3, addValue/5]).

-record(measurement, {date, type, value}).
-record(station, {coordinates, measurements}).
-record(monitor, {stations}).

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

addValue(#monitor { stations = Stations }, StationName, Date, MeasurementType, MeasurementValue) when is_list(StationName) and is_list(MeasurementType) and is_float(MeasurementValue) ->
  addValueCheckedStation(#monitor {stations =  Stations}, maps:is_key(StationName, Stations), StationName, Date, MeasurementType, MeasurementValue);
addValue(#monitor { stations = Stations }, Coordinates, Date, MeasurementType, MeasurementValue) when is_tuple(Coordinates) and is_list(MeasurementType) and is_float(MeasurementValue) ->
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
    true  -> {error, "Failed to add measured value: the measurement for this station, date, time and type has already been added."}
  end.

%%removeValue/4 - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
%%getOneValue/4 - zwraca wartość pomiaru o zadanym typie, z zadanej daty i stacji;
%%getStationMean/3 - zwraca średnią wartość parametru danego typu z zadanej stacji;
%%getDailyMean/3 - zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;
