-module(hearts).
-compile(debug_info).
-export([new_deck/0, legal_turn_cards/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_deck() ->
	cards:new_deck(cards:standard_suits(), lists:seq(2, 14)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

legal_turn_cards(Hand, [], _) when length(Hand) == 13 ->
	[{2, club}];
legal_turn_cards(Hand, _, _) when length(Hand) == 13 ->
	{Clubs, NonClubs} = cards:partition(Hand, club),
	NonPoints = cards:not_in_suit(NonClubs, heart) -- [{12, spade}],
	util:first_non_empty(Clubs, NonPoints, Hand);
legal_turn_cards(Hand, [], true) ->
	Hand;
legal_turn_cards(Hand, [], false) ->
	NonHearts = cards:not_in_suit(Hand, heart),
	util:first_non_empty(NonHearts, Hand, []);
legal_turn_cards(Hand, [{_, LeadSuit} | _], _) ->
	Suited = cards:in_suit(Hand, LeadSuit),
	util:first_non_empty(Suited, Hand, []).
