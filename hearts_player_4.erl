-module(hearts_player_4).
-behaviour(hearts_player).
-export([description/0, pick_pass_cards/3, pick_turn_card/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rachael Yamagata - Happenstance (2003)

description() ->
	"smart, random".
	
should_shoot(Hand, heart) ->
	;
should_shoot(Hand, heart) ->
	;
should_shoot(Hand, Suit) ->
	.

should_shoot(Hand) ->
	% If I ...
	%     - have all the highest cards
	%     - have the most hearts, plus enough high hearts
	%     - have most of any suit
	should_shoot_suits(Hand, cards:standard_suits()),
	{WillTake, ShouldTake, Risky, Worthless} = eval_suit_for_shoot(Hand, heart),
	{WillTake, ShouldTake, Risky, Worthless} = eval_suit_for_shoot(Hand, spade),
	{WillTake, ShouldTake, Risky, Worthless} = eval_suit_for_shoot(Hand, diamond),
	{WillTake, ShouldTake, Risky, Worthless} = eval_suit_for_shoot(Hand, club),
	.

pick_pass_cards(_Tally, Hand, _Position) ->
	pick_pass_cards(should_shoot(Hand), Hand).

pick_pass_cards(true, Hand) ->
	ok;
pick_pass_cards(false, Hand) ->
	ok.

pick_turn_card(_Tally, Hand, CardsPlayed, HeartsBroken) ->
	util:random(hearts:legal_turn_cards(Hand, CardsPlayed, HeartsBroken)).
