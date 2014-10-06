-module(hearts_game_server).
-export([init/1]).
-export([start_link/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Clients) ->
	% io:format("hearts_game_server ~p started: ~p~n", [self(), Clients]),
	send_to_clients(Clients, {start, self()}),
	{ok, play_game(Clients)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Clients) ->
	% gen_server:start_link(?MODULE, Clients, []).
	spawn_link(?MODULE, init, [Clients]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deal_hands([], [], _Pass) ->
	[];
deal_hands([Client | RemainingClients], Deck, Pass) ->
	{Hand, RemainingDeck} = lists:split(13, Deck),
	send_to_client(Client, {new_hand, Hand, Pass}),
	[Hand | deal_hands(RemainingClients, RemainingDeck, Pass)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_to_client({EndPoint, _}, Message) ->
	% io:format("~p ! ~p~n", [EndPoint, Message]),
	EndPoint ! Message.

send_to_clients([], _) ->
	ok;
send_to_clients([Client | RemainingClients], Message) ->
	send_to_client(Client, Message),
	send_to_clients(RemainingClients, Message).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_pass(Client, Hand, Pass) ->
	{EndPoint, _} = Client,
	receive
		{pass, From, PassCards} when From == EndPoint ->
			TempCards = Hand -- PassCards,
			if
				length(PassCards) /= 3 ->
					send_to_client(Client, not_three_cards),
					send_to_client(Client, {new_hand, Hand, Pass}),
					receive_pass(Client, Hand, Pass);
				length(TempCards) /= 10 ->
					send_to_client(Client, play_card_from_hand),
					send_to_client(Client, {new_hand, Hand, Pass}),
					receive_pass(Client, Hand, Pass);
				true ->
					PassCards
			end
	end.

receive_passes([], [], _Pass) ->
	[];
receive_passes([Client | RemainingClients], [Hand | RemainingHands], Pass) ->
	PassCards = receive_pass(Client, Hand, Pass),
	[PassCards | receive_passes(RemainingClients, RemainingHands, Pass)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_cards(CardsA, CardsB) ->
	lists:zipwith(fun(A, B) -> A -- B end, CardsA, CardsB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_cards(CardsA, CardsB) ->
	lists:zipwith(fun(A, B) -> A ++ B end, CardsA, CardsB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rotate_passes(Passes, left) ->
	util:rotate_up(Passes, 1);
rotate_passes(Passes, right) ->
	util:rotate_up(Passes, 3);
rotate_passes(Passes, across) ->
	util:rotate_up(Passes, 2).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pass(_Clients, keep, Hands) ->
	Hands;
pass(Clients, Pass, Hands) ->
	Passes = receive_passes(Clients, Hands, Pass),
	TempHands = remove_cards(Hands, Passes),
	Passed = rotate_passes(Passes, Pass),
	add_cards(TempHands, Passed).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_points(PointsA, PointsB) ->
	lists:zipwith(fun(A, B) -> A + B end, PointsA, PointsB).

add_points([PointsA | RemainingA], 1, PointsB) ->
	[PointsA + PointsB | RemainingA];
add_points([PointsA | RemainingA], Index, PointsB) ->
	[PointsA | add_points(RemainingA, Index - 1, PointsB)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_result({_, Name}, Points, MoonShots, {Results, LowestPoints}) ->
	{[{Name, Points, MoonShots, Points == LowestPoints} | Results], LowestPoints}.

find_results([], [], [], LowestPointsSoFar) ->
	{[], LowestPointsSoFar};
find_results([Client | RemainingClients], [Points | RemainingPoints], [MoonShots | RemainingMoonShots], LowestPointsSoFar) when Points < LowestPointsSoFar ->
	add_result(Client, Points, MoonShots, find_results(RemainingClients, RemainingPoints, RemainingMoonShots, Points));
find_results([Client | RemainingClients], [Points | RemainingPoints], [MoonShots | RemainingMoonShots], LowestPointsSoFar) ->
	add_result(Client, Points, MoonShots, find_results(RemainingClients, RemainingPoints, RemainingMoonShots, LowestPointsSoFar)).

find_results(Clients, Points, MoonShots) ->
	find_results(Clients, Points, MoonShots, 200).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_game(Clients) ->
	Results = play_hands(Clients),
	% hearts_lobby_server:show_winners(Results),
	hearts_lobby_server:store_result(Results),
	send_to_clients(Clients, {finish_game, Results}),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_hand(Clients, Pass) ->
	DealtHands = deal_hands(Clients, util:randomize(hearts:new_deck()), Pass),
	Hands = pass(Clients, Pass, DealtHands),
	{Points, MoonShots} = play_tricks(Clients, Hands),
	% io:format("~p play_hand(~p) == ~p~n", [self(), Pass, Points]),
	{Points, MoonShots}.

play_hands(Clients, [GamePoints1, GamePoints2, GamePoints3, GamePoints4], MoonShots, _Pass) when (GamePoints1 >= 100) or (GamePoints2 >= 100) or (GamePoints3 >= 100) or (GamePoints4 >= 100) ->
	GamePoints = [GamePoints1, GamePoints2, GamePoints3, GamePoints4],
	{Results, _} = find_results(Clients, GamePoints, MoonShots),
	Results;
play_hands(Clients, GamePoints, GameMoonShots, Pass) ->
	{HandPoints, HandMoonShots} = play_hand(Clients, Pass),
	send_to_clients(Clients, {finish_hand, HandPoints}),
	play_hands(Clients, add_points(GamePoints, HandPoints), add_points(GameMoonShots, HandMoonShots), rotate_pass(Pass)).

play_hands(Clients) ->
	play_hands(Clients, [0, 0, 0, 0], [0, 0, 0, 0], left).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rotate_pass(left) ->
	right;
rotate_pass(right) ->
	across;
rotate_pass(across) ->
	keep;
rotate_pass(keep) ->
	left.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_lead_suit([{_, LeadSuit} | _]) ->
	LeadSuit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_suit(Cards, Suit) ->
	lists:foldl(
		fun(
			{_, CardSuit}, _) when CardSuit == Suit -> true;
			(_, Result) -> Result
		end, false, Cards).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take_turn(Client, Hand, CardsPlayed, HeartsBroken) ->
	send_to_client(Client, {take_turn, Hand, CardsPlayed, HeartsBroken}),
	{EndPoint, _} = Client,
	receive
		{take_turn, From, Card} when From == EndPoint ->
			{_, Suit} = Card,
			PlayCardFromHand = not lists:member(Card, Hand),
			FollowSuit = if
				length(CardsPlayed) == 0 ->
					false;
				true ->
					LeadSuit = get_lead_suit(CardsPlayed),
					if
						Suit == LeadSuit ->
							false;
						true ->
							has_suit(Hand, LeadSuit)
					end
			end,
			PlayDeuceOfClubs = if
				Card == {2,club} ->
					false;
				length(CardsPlayed) /= 0 ->
					false;
				length(Hand) /= 13 ->
					false;
				true ->
					true
			end,
			NoPointsFirstTrick = if
				length(Hand) /= 13 ->
					false;
				Card == {12,spade} ->
					true;
				Suit == heart ->
					true;
				true ->
					false
			end,
			NoHeartsUntilBroken = if
				HeartsBroken ->
					false;
				Suit /= heart ->
					false;
				length(CardsPlayed) /= 0 ->
					false;
				true ->
					has_suit(Hand, spade) or has_suit(Hand, diamond) or has_suit(Hand, club)
			end,
			if
				PlayCardFromHand ->
					send_to_client(Client, play_card_from_hand),
					take_turn(Client, Hand, CardsPlayed, HeartsBroken);
				PlayDeuceOfClubs ->
					send_to_client(Client, play_deuce_of_clubs),
					take_turn(Client, Hand, CardsPlayed, HeartsBroken);
				FollowSuit ->
					send_to_client(Client, follow_suit),
					take_turn(Client, Hand, CardsPlayed, HeartsBroken);
				NoPointsFirstTrick ->
					send_to_client(Client, no_points_first_trick),
					take_turn(Client, Hand, CardsPlayed, HeartsBroken);
				NoHeartsUntilBroken ->
					send_to_client(Client, no_hearts_until_broken),
					take_turn(Client, Hand, CardsPlayed, HeartsBroken);
				true ->
					Card
			end
	end.
	
take_turns(_Clients, _Hands, _Player, CardsPlayed, _HeartsBroken) when length(CardsPlayed) == 4 ->
	CardsPlayed;
take_turns(Clients, Hands, Player, CardsPlayed, HeartsBroken) ->
	Client = lists:nth(Player, Clients),
	Hand = lists:nth(Player, Hands),
	Card = take_turn(Client, Hand, CardsPlayed, HeartsBroken),
	NewCardsPlayed = CardsPlayed ++ [Card],
	take_turns(Clients, Hands, (Player rem 4) + 1, NewCardsPlayed, HeartsBroken).

take_turns(Clients, Hands, Leader, HeartsBroken) ->
	take_turns(Clients, Hands, Leader, [], HeartsBroken).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_winner(LeadSuit, Cards) ->
	{_, Index, _} = lists:foldl(
		fun
			({Ordinal, Suit}, {CurrentIndex, _, HighOrdinal}) when Suit == LeadSuit, Ordinal > HighOrdinal ->
				{CurrentIndex + 1, CurrentIndex, Ordinal};
			(_, {CurrentIndex, HighIndex, HighOrdinal}) ->
				{CurrentIndex + 1, HighIndex, HighOrdinal}
		end, {1, 0, 0}, Cards),
		Index.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count_points(Cards) ->
	lists:foldl(
		fun
			({_, heart}, Points) ->
				Points + 1;
			({12, spade}, Points) ->
				Points + 13;
			(_, Points) ->
				Points
		end, 0, Cards).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_leader(true, N, _) ->
	N;
find_leader(false, N, [Hand | RemainingHands]) ->
	find_leader(lists:member({2,club}, Hand), N + 1, RemainingHands).

find_leader(Hands) ->
	find_leader(false, 0, Hands).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_trick_result_to_clients([], _, _, _, _) ->
	ok;
send_trick_result_to_clients([Client | RemainingClients], CardsPlayed, [Leader | RemainingLeaders], [TrickWinner | RemainingTrickWinners], TrickPoints) ->
	send_to_client(Client, {finish_trick, CardsPlayed, Leader, TrickWinner, TrickPoints}),
	send_trick_result_to_clients(RemainingClients, CardsPlayed, RemainingLeaders, RemainingTrickWinners, TrickPoints);
send_trick_result_to_clients(Clients, CardsPlayed, Leader, TrickWinner, TrickPoints) ->
	RelativeLeaders = util:rotate_up([me, right, across, left], Leader - 1),
	RelativeWinners = util:rotate_up([me, right, across, left], TrickWinner - 1),
	send_trick_result_to_clients(Clients, CardsPlayed, RelativeLeaders, RelativeWinners, TrickPoints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_tricks(13, _Clients, _Hands, _Leader, [26, 0, 0, 0], _HeartsBroken) ->
	{[0, 26, 26, 26], [1, 0, 0, 0]};
play_tricks(13, _Clients, _Hands, _Leader, [0, 26, 0, 0], _HeartsBroken) ->
	{[26, 0, 26, 26], [0, 1, 0, 0]};
play_tricks(13, _Clients, _Hands, _Leader, [0, 0, 26, 0], _HeartsBroken) ->
	{[26, 26, 0, 26], [0, 0, 1, 0]};
play_tricks(13, _Clients, _Hands, _Leader, [0, 0, 0, 26], _HeartsBroken) ->
	{[26, 26, 26, 0], [0, 0, 0, 1]};
play_tricks(13, _Clients, _Hands, _Leader, HandPoints, _HeartsBroken) ->
	{HandPoints, [0, 0, 0, 0]};
play_tricks(N, Clients, Hands, Leader, HandPoints, HeartsBroken) when N < 13 ->
	CardsPlayed = take_turns(Clients, Hands, Leader, HeartsBroken),
	LeadSuit = get_lead_suit(CardsPlayed),
	TrickPoints = count_points(CardsPlayed),
	NewHeartsBroken = HeartsBroken or has_suit(CardsPlayed, heart),
	PlayerCards = util:rotate_up(CardsPlayed, Leader - 1),
	TrickWinner = find_winner(LeadSuit, PlayerCards),
	NewHands = remove_cards(Hands, [[X] || X <- PlayerCards]),
	NewHandPoints = add_points(HandPoints, TrickWinner, TrickPoints),
	send_trick_result_to_clients(Clients, CardsPlayed, Leader, TrickWinner, TrickPoints),
	% io:format("~p ~p~n", [self(), NewHandPoints]),
	play_tricks(N + 1, Clients, NewHands, TrickWinner, NewHandPoints, NewHeartsBroken).

play_tricks(Clients, Hands) ->
	play_tricks(0, Clients, Hands, find_leader(Hands), [0, 0, 0, 0], false).
