%%%-------------------------------------------------------------------
%%% @author Jan Wilczek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2019 6:21 AM
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Jan Wilczek").
-include_lib("eunit/include/eunit.hrl").
-include("pollution.hrl").
-compile(export_all).

createMonitor_test() -> #monitor{ stations=#{} } = pollution:createMonitor().
addStation_test() -> #monitor{ stations = Stations } = pollution:addStation(pollution:createMonitor(), "Aleje", {1, 2.0}),
  ?assert(maps:is_key("Aleje", Stations)),
  ?assertEqual(maps:get("Aleje", Stations), #station{ coordinates = {1, 2.0}, measurements = []}).

setup_test() ->
  {setup,
    fun () -> pollution:createMonitor() end,
    [fun addGetValue_test/1]}.

setUp() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation(M1, "New York", {-81.3, 52.3}),
  M2.

addGetValue_test(Monitor) -> M = pollution:addValue(Monitor, "Valley", {{2019, 5, 9},{7,12,0}}, "PM10", 100.1),
  ?assertEqual(100.1, pollution:getOneValue(Monitor, "Valley", {{2019, 5, 9}, {7,12,0}}, "PM10")),
  ?assertEqual(100.1, pollution:getOneValue(Monitor, {-81.3, 52.3}, {{2019, 5, 9}, {7,12,0}}, "PM10")).
