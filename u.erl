-module(u).
-export([c/0, p/0, g/0, s/0, r/0, p/1]).

c() ->
	c:nc(u, [debug_info]),
	c:nc(cards, [debug_info]),
	c:nc(hearts, [debug_info]),
	c:nc(hearts_lobby_server, [debug_info]),
	c:nc(hearts_game_server, [debug_info]),
	c:nc(hearts_player, [debug_info]),
	c:nc(hearts_player_1, [debug_info]),
	c:nc(hearts_player_2, [debug_info]),
	c:nc(hearts_player_3, [debug_info]),
	c:nc(hearts_player_4, [debug_info]),
	c:nc(hearts_app, [debug_info]),
	c:nc(util, [debug_info]),
	r().

p(0) ->
	ok;
p(N) ->
	hearts_player:spawn(hearts_player_1, N),
	hearts_player:spawn(hearts_player_2, N),
	hearts_player:spawn(hearts_player_3, N),
	hearts_player:spawn(hearts_player_4, N).

r() ->
	hearts_lobby_server:start_link(),
	hearts_lobby_server:reset().

s() ->
	{_, Stats} = hearts_lobby_server:state(),
	hearts_lobby_server:render_stats(Stats).

p() ->
	p(1).

g() ->
	c(),
	p().