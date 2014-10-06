-module(hearts_player_2).
-behaviour(hearts_player).
-export([description/0, pick_pass_cards/3, pick_turn_card/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

description() ->
	"high, no_points".

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

pick_turn_card_2(Hand, [], _) when length(Hand) == 13 ->
	{2,club};
pick_turn_card_2([{Ordinal, Suit} | _], [{_, LeadSuit} | _], _) when Suit == LeadSuit ->
	{Ordinal, Suit};
pick_turn_card_2([], _, NonHeart) ->
	NonHeart;
pick_turn_card_2([Card], _, false) ->
	Card;
pick_turn_card_2([{Ordinal, Suit} | RemainingCards], CardsPlayed, _) when (Suit /= heart) and ({Ordinal, Suit} /= {12,spade}) ->
	pick_turn_card_2(RemainingCards, CardsPlayed, {Ordinal, Suit});
pick_turn_card_2([{Ordinal, Suit} | RemainingCards], CardsPlayed, false) when (Suit /= heart) ->
	pick_turn_card_2(RemainingCards, CardsPlayed, {Ordinal, Suit});
pick_turn_card_2([_ | RemainingCards], CardsPlayed, NonHeart) ->
	pick_turn_card_2(RemainingCards, CardsPlayed, NonHeart).

pick_turn_card(_Tally, Hand, CardsPlayed, _HeartsBroken) ->
	pick_turn_card_2(Hand, CardsPlayed, false).
