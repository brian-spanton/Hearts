-module(hearts_player_3).
-behaviour(hearts_player).
-export([description/0, pick_pass_cards/3, pick_turn_card/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

description() ->
	"random, random".

pick_pass_cards(_Tally, Hand, _Position) ->
	{Three, _} = lists:split(3, util:randomize(Hand)),
	Three.

pick_turn_card(_Tally, Hand, CardsPlayed, HeartsBroken) ->
	util:random(hearts:legal_turn_cards(Hand, CardsPlayed, HeartsBroken)).
