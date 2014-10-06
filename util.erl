-module(util).
-compile(debug_info).
-export([randomize/1, first_non_empty/3, rotate_down/2, rotate_up/2, rotate_to/2, random/1, random/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

randomize(List) ->
	{V1, V2, V3} = now(),
	random:seed(V1, V2, V3),
	Temp = [{Item, Order} || Item <- List, Order <- [random:uniform()]],
	[Item || {Item, _} <- lists:sort(fun({_, A}, {_, B}) -> A > B end, Temp)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

first_non_empty([], [], List) ->
	List;
first_non_empty([], List, _) ->
	List;
first_non_empty(List, _, _) ->
	List.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rotate_down(List, 0) ->
	List;
rotate_down(List, N) when N < 0 ->
	rotate_down(List, N + length(List));
rotate_down(List, N) when N >= length(List) ->
	rotate_down(List, N rem length(List));
rotate_down([A | B], 1) ->
	B ++ [A];
rotate_down(List, N) ->
	{A, B} = lists:split(N, List),
	B ++ A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rotate_up(List, 0) ->
	List;
rotate_up(List, N) when N < 0 ->
	rotate_up(List, N + length(List));
rotate_up(List, N) when N >= length(List) ->
	rotate_up(List, N rem length(List));
rotate_up(List, N) ->
	rotate_down(List, length(List) - N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rotate_to(A, [B1 | B], Item) when B1 == Item ->
	[B1 | B] ++ A;
rotate_to(A, [B1 | B], Item) ->
	rotate_to(A ++ [B1], B, Item).
	
rotate_to(List, Item) ->
	rotate_to([], List, Item).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random(List) ->
	[Result | _] = util:randomize(List),
	Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random(List, N) ->
	{Result, _} = lists:split(N, util:randomize(List)),
	Result.
