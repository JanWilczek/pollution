%%%-------------------------------------------------------------------
%%% @author Jan Wilczek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2019 6:37 AM
%%%-------------------------------------------------------------------
-author("Jan Wilczek").

-record(measurement, {date, type, value}).
-record(station, {coordinates, measurements}).
-record(monitor, {stations}).
