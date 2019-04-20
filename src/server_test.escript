#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose

-include("pollution_server.erl").

main(_) ->
  pollution_server:start(),
  pollution_server:addStation("Aleje", {1,2}),
  pollution_server:addValue("Kamionka", calendar:local_time(), "PM10", 1.0),
  pollution_server:addStation("Kamionka", {4,5.0}),
  pollution_server:addValue("Aleje", calendar:local_time(), "PM10", 4.0),
  pollution_server:addValue("Aleje", calendar:local_time(), "PM10", 5.0),
  pollution_server:addValue("Kamia", calendar:local_time(), "PM10", 1.0),
  pollution_server:addValue({1,2}, calendar:local_time(), "PM10", 6.0),
  pollution_server:addValue("Aleje", {{2019,4,18}, {16,21,00}}, "PM10", 2),
  pollution_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_server:removeValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_server:getStationMean("Aleje", "PM10"),
  pollution_server:getStationMean("Kamionka", "PM10"),
  pollution_server:getOneValue("Aleje", {{2019, 4, 18}, {16,21,00}}, "PM10"),
  pollution_server:stop().
