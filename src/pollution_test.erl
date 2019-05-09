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

setup_test_() ->
  {setup,
    fun () -> setUp() end,
    fun () -> [fun addGetValue_test/1, fun removeValue_test/1] end}.

setUp() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation(M1, "New York", {-81.3, 52.3}),
  M2.

addGetValue_test(Monitor) -> M = pollution:addValue(Monitor, "Valley", {{2019, 5, 9},{7,12,0}}, "PM10", 100.1),
  ?assertEqual(100.1, pollution:getOneValue(M, "Valley", {{2019, 5, 9}, {7,12,0}}, "PM10")),
  ?assertEqual(100.1, pollution:getOneValue(M, {-81.3, 52.3}, {{2019, 5, 9}, {7,12,0}}, "PM10")).

removeValue_test(Monitor) -> M = pollution:addValue(Monitor, "Valley", {{2019, 5, 9},{7,12,0}}, "PM10", 100.1),
  M2 = pollution:removeValue(M, "Valley", {{2019,5,9}, {7,12,0}}, "PM10"),
  {Atom, _} = pollution:getOneValue(M2, "Valley", {{2019, 5,9}, {7,12,0}}, "PM10"),
  ?assertEqual(Atom, error).
