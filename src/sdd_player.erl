%%% =================================================================================== %%%
%%% Sudoku Dash Game: Player Server                                                     %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is a gen_server that handles a connected player:                               %%%
%%% - holds temporary player state                                                      %%%
%%% - connects game and client                                                          %%%
%%% It only lives as long as the client processes it is connected to although it can    %%%
%%% handle temporary disconnects of the client.                                         %%%
%%% =================================================================================== %%%

-module(sdd_player).

%% API
-export([handle_game_event/3]).

%% Records
-record(state, {
	name,
	secret,
	current_game,
	points,
	badges = []
}).

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

handle_game_event(PlayerId, EventType, EventData) ->
	try gen_server:call(PlayerId, {game_event, EventType, EventData}, 100) of
		Reply -> Reply
	catch
		Error -> Error
	end.

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Creates a history for a new player, assigns the realizer function to it
%% and adds a register event with the given player information

init(PlayerInfo) ->
	EmptyHistory = sdd_history:new(fun realize_event/3),
	InitialHistory = sdd_history:append(EmptyHistory, register, PlayerInfo),
	{ok, InitialHistory}.

%% ------------------------------------------------------------------------------------- %%
%% Tries to join a game, coming from a source

handle_cast({join, {GameId, Source}}, History) ->
	State = sdd_history:state(History),
	case sdd_game:join(State#state.name, GameId, Source) of
		ok ->
			NewHistory = sdd_history:append(History, join, {GameId, Source}),
			{noreply, NewHistory};
		_Error ->
			{noreply, History}
	end;

%% Leaves the current game, for a reason

handle_cast({leave, Reason}, History) ->
	NewHistory = sdd_history:append(History, leave, Reason),
	{noreply, NewHistory}.

%% ------------------------------------------------------------------------------------- %%
%% Returns continue_listening for events from our current game
%% And saves own positive guess results

handle_call({game_event, GameId, EventType, EventData}, History) ->
	State = sdd_history:state(History),
	CurrentGame = State#state.current_game,
	MyName = State#state.name ,
	case GameId of
		CurrentGame ->
			case EventType of
				guess ->
					{Name, _Position, _Number, Result} = EventData,
					case Name of
						MyName ->
							case Result of
								{good} ->
									NewHistory = sdd_history:append(History, get_guess_reward, Result),
									{continue_listening, NewHistory};
								_NotGood ->	
									{continue_listening, History}
							end;
						_SomeoneElse ->
							{continue_listening, History}
					end;
				_OtherEvent ->
					{continue_listening, History}
				end;
		_WrongGame ->
			{wrong_game, History}
	end;

%% ------------------------------------------------------------------------------------- %%
%% Adds a badge

handle_call({get_badge, Badge}, History) ->
	NewHistory = sdd_history:append(History, get_badge, Badge),
	{noreply, NewHistory}.

%%% =================================================================================== %%%
%%% HISTORY CALLBACKS                                                                   %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Create initial state

realize_event(_EmptyState, register, {Name, Secret}) ->
	#state{name = Name, secret = Secret, points = 0};

%% Increase points on good guess result

realize_event(State, get_guess_reward, _Result) ->
	State#state{points = State#state.points + 1};

%% Change current game

realize_event(State, join, {GameId, _Source}) ->
	State#state{current_game = GameId};

%% Reset current game

realize_event(State, leave, _Reason) ->
	State#state{current_game = undefined};

%% Add a badge

realize_event(State, get_badge, Badge) ->
	State#state{badges = [Badge | State#state.badges]}.


%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

init_createsNewPlayerWithNameAndSecret_test() ->
	{ok, InitialHistory} = init({"Peter", "secret"}),
	State = sdd_history:state(InitialHistory),
	?assertEqual(State#state.name, "Peter"),
	?assertEqual(State#state.secret, "secret"),
	?assertEqual(State#state.points, 0),

	Past = sdd_history:past(InitialHistory),
	?assertMatch([{_Time, register, {"Peter", "secret"}}], Past).

join_notifiesGameWeWantToJoinAndSavesGameAsCurrentGameIfSuccessful_test() ->
	meck:new(sdd_game),
	meck:expect(sdd_game, join, fun
		("Peter", "GoodGame", invite) -> ok;
		("Peter", "BadGame", random) -> nope
	end),

	{ok, InitialHistory} = init({"Peter", "secret"}),

	{noreply, HistoryAfterBadJoin} = handle_cast({join, {"BadGame", random}}, InitialHistory),
	?assertEqual(HistoryAfterBadJoin, InitialHistory),

	?assert(meck:called(sdd_game, join, ["Peter", "BadGame", random])),

	{noreply, HistoryAfterGoodJoin} = handle_cast({join, {"GoodGame", invite}}, InitialHistory),

	?assert(meck:called(sdd_game, join, ["Peter", "GoodGame", invite])),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),

	State = sdd_history:state(HistoryAfterGoodJoin),
	?assertEqual(State#state.current_game, "GoodGame"),

	Past = sdd_history:past(HistoryAfterGoodJoin),
	?assertMatch([{_Time, join, {"GoodGame", invite}} | _], Past).

leave_resetsCurrentGame_test() ->
	meck:new(sdd_game),
	meck:expect(sdd_game, join, fun
		("Peter", "GoodGame", invite) -> ok
	end),

	{ok, InitialHistory} = init({"Peter", "secret"}),
	{noreply, HistoryAfterGoodJoin} = handle_cast({join, {"GoodGame", invite}}, InitialHistory),

	meck:unload(sdd_game),

	{noreply, HistoryAfterLeaving} = handle_cast({leave, timeout}, HistoryAfterGoodJoin),
	
	State = sdd_history:state(HistoryAfterLeaving),
	?assertEqual(State#state.current_game, undefined),

	Past = sdd_history:past(HistoryAfterLeaving),
	?assertMatch([{_Time, leave, timeout} | _ ], Past).

leave_notifiesGameThatWeLeft_test() ->	
	meck:new(sdd_game),
	meck:expect(sdd_game, join, fun
		("Peter", "GoodGame", invite) -> ok
	end),

	{ok, InitialHistory} = init({"Peter", "secret"}),
	{noreply, HistoryAfterGoodJoin} = handle_cast({join, {"GoodGame", invite}}, InitialHistory),
	?assert(meck:validate(sdd_game)),

	meck:expect(sdd_game, leave, fun
		("Peter", "GoodGame", timeout) -> ok
	end),

	{noreply, _} = handle_cast({leave, timeout}, HistoryAfterGoodJoin),

	?assert(meck:called(sdd_game, leave, '_')),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

handle_game_event_continuesListeningOnlyIfEventWasFromCurrentGame_test() ->
	meck:new(sdd_game),
	meck:expect(sdd_game, join, fun
		(_PlayerId, "GoodGame", _Source) -> ok
	end),

	{ok, InitialHistory} = init({"Peter", "secret"}),
	{noreply, HistoryAfterGoodJoin} = handle_cast({join, {"GoodGame", invite}}, InitialHistory),

	meck:unload(sdd_game),

	?assertMatch(
		{continue_listening, _NewHistory},
		handle_call({game_event, "GoodGame", some_event, some_data}, HistoryAfterGoodJoin)
	),
	?assertMatch(
		{wrong_game, _NewHistory},
		handle_call({game_event, "OtherGame", some_event, some_data}, HistoryAfterGoodJoin)
	).

handle_game_event_guess_SavesOwnPositiveGuessResultAndIncreasesPoints_test() ->
	meck:new(sdd_game),
	meck:expect(sdd_game, join, fun
		(_PlayerId, "GoodGame", _Source) -> ok
	end),

	{ok, InitialHistory} = init({"Peter", "secret"}),
	{noreply, HistoryAfterGoodJoin} = handle_cast({join, {"GoodGame", invite}}, InitialHistory),

	meck:unload(sdd_game),

	{continue_listening, HistoryAfterSomeonesGuess} = handle_call({game_event, "GoodGame", guess, {"SomeoneElse", 34, 3, {good}}}, HistoryAfterGoodJoin),
	?assertEqual(HistoryAfterSomeonesGuess, HistoryAfterGoodJoin),

	{continue_listening, HistoryAfterOwnNegativeGuess} = handle_call({game_event, "GoodGame", guess, {"Peter", 34, 4, {not_good}}}, HistoryAfterGoodJoin),
	?assertEqual(HistoryAfterOwnNegativeGuess, HistoryAfterGoodJoin),	

	{continue_listening, HistoryAfterOwnPositiveGuess} = handle_call({game_event, "GoodGame", guess, {"Peter", 34, 3, {good}}}, HistoryAfterGoodJoin),
	
	State = sdd_history:state(HistoryAfterOwnPositiveGuess),
	?assertEqual(State#state.points, 1),
	
	Past = sdd_history:past(HistoryAfterOwnPositiveGuess),
	?assertMatch([{_Time, get_guess_reward, {good}} | _ ], Past).

get_badge_addsABadge_test() ->
	{ok, InitialHistory} = init({"Peter", "secret"}),

	Badge1 = {"Good Test Subject", "For being an important part of these unit tests"},
	Badge2 = {"Good Person", "For having a beautiful personality"},

	{noreply, HistoryAfterGettingFirstBadge} = handle_call({get_badge, Badge1}, InitialHistory),
	{noreply, HistoryAfterGettingSecondBadge} = handle_call({get_badge, Badge2}, HistoryAfterGettingFirstBadge),
	
	State = sdd_history:state(HistoryAfterGettingSecondBadge),
	?assertEqual(State#state.badges, [Badge2, Badge1]),

	Past = sdd_history:past(HistoryAfterGettingSecondBadge),
	?assertMatch([{_Time2, get_badge, Badge2}, {_Time1, get_badge, Badge1} | _ ], Past).

-endif.