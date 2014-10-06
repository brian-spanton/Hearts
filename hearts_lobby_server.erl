-module(hearts_lobby_server).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([start_link/0, join/1, leave/0, stop/0, store_result/1, reset/0, state/0, render_stats/1]).

init(State) ->
	% io:format("hearts_lobby_server ~p started~n", [self()]),
	{ok, State}.

terminate(normal, _Players) ->
	% $briansp: notify players?
    ok.

handle_join(Players) when length(Players) == 4 -> 
	% io:format("spawning game~n"),
	hearts_game_server:start_link(Players),
	[];
handle_join(Players) -> 
	Players.

add_stat(NewStat, false) ->
	NewStat;
add_stat({Name, GamesA, PointsA, MoonShotsA, WinsA}, {_, GamesB, PointsB, MoonShotsB, WinsB}) ->
	{Name, GamesA + GamesB, PointsA + PointsB, MoonShotsA + MoonShotsB, WinsA + WinsB}.
	
store_result({Name, Points, MoonShots, Win}, Stats) ->
	Wins = case Win of true -> 1; false -> 0 end,
	StatA = {Name, 1, Points, MoonShots, Wins},
	StatB = lists:keyfind(Name, 1, Stats),
	lists:keystore(Name, 1, Stats, add_stat(StatA, StatB)).

store_results([], Stats) ->
	Stats;
store_results([Result | RemainingResults], Stats) ->
	store_results(RemainingResults, store_result(Result, Stats)).

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call({result, Results}, _From, {Players, OldStats}) ->
	NewStats = store_results(Results, OldStats),
	{reply, ok, {Players, NewStats}};
handle_call({join, Name}, From, {OldPlayers, Stats}) -> 
	{EndPoint, _Tag} = From,
	NewPlayers = lists:keystore(EndPoint, 1, OldPlayers, {EndPoint, Name}),
	% io:format("player ~p joined~n", [EndPoint]),
	RemainingPlayers = handle_join(NewPlayers),
	{reply, ok, {RemainingPlayers, Stats}};
handle_call(leave, From, {OldPlayers, Stats}) -> 
	{EndPoint, _Tag} = From,
	NewPlayers = lists:keydelete(EndPoint, 1, OldPlayers),
	% io:format("player ~p left~n", [EndPoint]),
	{reply, ok, {NewPlayers, Stats}}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(reset, _) ->
    {noreply, new_state()}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

new_state() ->
	{[], []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({global, hearts_lobby_server}, ?MODULE, new_state(), []).

join(Name) ->
	gen_server:call({global, hearts_lobby_server}, {join, Name}).

leave() ->
	gen_server:call({global, hearts_lobby_server}, leave).

stop() ->
    gen_server:cast({global, hearts_lobby_server}, stop).

store_result(Results) ->
    gen_server:call({global, hearts_lobby_server}, {result, Results}).

reset() ->
	gen_server:cast({global, hearts_lobby_server}, reset).
	
state() ->
	gen_server:call({global, hearts_lobby_server}, state).

render_stats(Stats) ->
	io:format("~-24s ~8s ~8s ~8s ~8s~n", ["Name", "Games", "Points/G", "Shots/G", "Win %"]),
	render_stats_rows(lists:sort(Stats)).
	
render_stats_rows([]) ->
	ok;
render_stats_rows([{Name, Games, Points, Shots, Wins} | RemainingRows]) ->
	io:format("~-24s ~8w ~8.1f ~8.1f ~8.1f~n", [Name, Games, (Points/Games), (Shots/Games), (Wins*100/Games)]),
	render_stats_rows(RemainingRows).
