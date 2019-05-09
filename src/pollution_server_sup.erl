%%%-------------------------------------------------------------------
%%% @author Jan Wilczek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2019 9:52 AM
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("Jan Wilczek").

%% API
-export([start/0, loop/0]).

start() ->
  spawn(?MODULE, loop, []).

loop() ->
  process_flag(trap_exit, true),
  pollution_server:start(),
  receive
    {'EXIT', _, normal} -> ok;
    {'EXIT', _, _} -> loop()
  end.
