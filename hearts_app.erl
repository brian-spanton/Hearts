-module(hearts_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    hearts_server:start_link().

stop(_State) ->
    hearts_server:stop().
