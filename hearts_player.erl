-module(hearts_player).
-export([behaviour_info/1, spawn/2, join/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% behaviour
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

behaviour_info(callbacks) ->
    [{description,0}, {pick_pass_cards,3}, {pick_turn_card,4}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% entry points
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spawn(Module, Games) ->
	spawn(?MODULE, join, [Module, Games]).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state machine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join(_, 0) ->
	ok;
join(Module, N) ->
	hearts_lobby_server:join(Module:description()),
	start(Module, N).

start(Module, N) ->
	receive
		{start, Host} -> new_hand(Module, N, Host)
	end.
	
send_pass_cards(_, _, Tally, _, keep) ->
	Tally;
send_pass_cards(Module, Host, Tally, Hand, Position) ->
	Three = Module:pick_pass_cards(Tally, Hand, Position),
	NewTally = set_cards(Tally, Three, [Position]),
	Host ! {pass, self(), Three},
	NewTally.

new_hand(Module, N, Host) ->
	receive
		{new_hand, Hand, Position} ->
			Tally = set_cards(new_tally(), Hand, [me]),
			State = send_pass_cards(Module, Host, Tally, Hand, Position),
			take_turn(Module, N, Host, State);
		{finish_game, _Results} -> join(Module, N)
	end.

send_turn_cards(Module, Host, Tally, HandCards, CardsPlayed, HeartsBroken) ->
	Host ! {take_turn, self(), Module:pick_turn_card(Tally, HandCards, CardsPlayed, HeartsBroken)}.

take_turn(Module, N, Host, Tally) ->
	receive
		{take_turn, HandCards, CardsPlayed, HeartsBroken} ->
			Players = util:rotate_up([me, left, across, right], length(CardsPlayed)),
			NewTally = tally_cards(Tally, CardsPlayed, Players),
			send_turn_cards(Module, Host, NewTally, HandCards, CardsPlayed, HeartsBroken),
			finish_trick(Module, N, Host, NewTally);
		{finish_hand, _Results} -> new_hand(Module, N, Host)
	end.
	
finish_trick(Module, N, Host, Tally) ->
	receive
		{finish_trick, CardsPlayed, TrickLeader, _TrickWinner, _TrickPoints} ->
			Players = util:rotate_to([me, left, across, right], TrickLeader),
			% io:format("~p ~p ~p ~p~n", [self(), CardsPlayed, Players, TrickLeader]),
			NewTally = tally_cards(Tally, CardsPlayed, Players),
			% io:format("~p ~p~n", [self(), NewTally]),
			take_turn(Module, N, Host, NewTally);
		Message -> io:format("~p ~p received~n", [self(), Message])
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% general
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tally_cards(Tally, [], _) ->
	Tally;
tally_cards(Tally, [CardPlayed], _) ->
	set_card(Tally, CardPlayed, []);
tally_cards(Tally, CardsPlayed, [_ | RemainingPlayers]) ->
	NewTally = set_cards(Tally, CardsPlayed, []),
	[{_, LeadSuit} | RemainingCards] = CardsPlayed,
	check_voided(NewTally, LeadSuit, RemainingCards, RemainingPlayers).

check_voided(Tally, _, [], _) ->
	Tally;
check_voided(Tally, LeadSuit, [{_, Suit} | RemainingCards], [_ | RemainingPlayers]) when Suit == LeadSuit ->
	check_voided(Tally, LeadSuit, RemainingCards, RemainingPlayers);
check_voided(Tally, LeadSuit, [_ | RemainingCards], [Player | RemainingPlayers]) ->
	check_voided(void_suit(Tally, LeadSuit, Player), LeadSuit, RemainingCards, RemainingPlayers).

void_suit(Processed, [], _, _) ->
	Processed;
void_suit(Processed, [{{Ordinal, CardSuit}, Players} | Unprocessed], Suit, Player) when CardSuit == Suit ->
	Card = {Ordinal, CardSuit},
	NewPlayers = Players -- [Player],
	Item = {Card, NewPlayers},
	void_suit(Processed ++ [Item], Unprocessed, Suit, Player);
void_suit(Processed, [Item | Unprocessed], Suit, Player) ->
	void_suit(Processed ++ [Item], Unprocessed, Suit, Player).

void_suit(Tally, Suit, Player) ->
	void_suit([], Tally, Suit, Player).

new_tally() ->
	Deck = hearts:new_deck(),
	CardState = lists:duplicate(length(Deck), [left,right,across]),
	lists:zip(Deck, CardState).

set_card(Tally, Card, Value) ->
	lists:keyreplace(Card, 1, Tally, {Card, Value}).
	
set_cards(Tally, [], _) ->
	Tally;
set_cards(Tally, [Card | RemainingCards], Value) ->
	set_cards(set_card(Tally, Card, Value), RemainingCards, Value).
