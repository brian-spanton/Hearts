-module(hearts_player_1).
-behaviour(hearts_player).
-export([description/0, pick_pass_cards/3, pick_turn_card/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

description() ->
	"high, random".

pick_pass_cards(_Tally, Hand, _Position) ->
	Ordered = lists:sort(
		fun
			({O1, _}, {O2, _}) when O1 > O2 ->
				true;
			(_, _) ->
				false
		end, Hand),
	{Three, _} = lists:split(3, Ordered),
	Three.

pick_turn_card(_Tally, Hand, CardsPlayed, HeartsBroken) ->
	util:random(hearts:legal_turn_cards(Hand, CardsPlayed, HeartsBroken)).
