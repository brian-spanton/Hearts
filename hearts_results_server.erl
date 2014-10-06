-module(hearts_results_server).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([start_link/0, join/1, leave/0, stop/0]).

init(Players) ->
	% io:format("hearts_results_server ~p started~n", [self()]),
	{ok, Players}.

terminate(normal, _Players) ->
	% $briansp: notify players?
    ok.

handle_join(Players) when length(Players) == 4 -> 
	% io:format("spawning game~n"),
	hearts_game_server:start_link(Players),
	[];
handle_join(Players) -> 
	Players.

handle_call({join, Name}, From, OldPlayers) -> 
	{EndPoint, _Tag} = From,
	NewPlayers = lists:keystore(EndPoint, 1, OldPlayers, {EndPoint, Name}),
	% io:format("player ~p joined~n", [EndPoint]),
	RemainingPlayers = handle_join(NewPlayers),
	{reply, ok, RemainingPlayers};
handle_call(leave, From, OldPlayers) -> 
	{EndPoint, _Tag} = From,
	NewPlayers = lists:keydelete(EndPoint, 1, OldPlayers),
	% io:format("player ~p left~n", [EndPoint]),
	{reply, ok, NewPlayers}.

handle_cast(stop, OldPlayers) ->
    {stop, normal, OldPlayers}.

start_link() ->
	gen_server:start_link({global, hearts_results_server}, ?MODULE, [], []).

join(Name) ->
	gen_server:call({global, hearts_results_server}, {join, Name}).

leave() ->
	gen_server:call({global, hearts_results_server}, leave).

stop() ->
    gen_server:cast({global, hearts_results_server}, stop).
