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
	current_client,
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

%% Leaves the current game, for a reason, and notifies the game

handle_cast({leave, Reason}, History) ->
	State = sdd_history:state(History),
	sdd_game:leave(State#state.name, State#state.current_game, Reason),
	NewHistory = sdd_history:append(History, leave, Reason),
	{noreply, NewHistory}.

%% ------------------------------------------------------------------------------------- %%
%% Returns continue_listening for events from our current game
%% And saves own positive guess results

handle_call({game_event, GameId, EventType, EventData}, History) ->
	State = sdd_history:state(History),
	CurrentGame = State#state.current_game,
	MyName = State#state.name,
	case GameId of
		CurrentGame ->
			case State#state.current_client of
				undefined -> do_nothing;
				ClientId -> sdd_client:handle_event(ClientId, game_event, GameId, EventType, EventData)
			end,
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
	{noreply, NewHistory};

%% ------------------------------------------------------------------------------------- %%
%% Connects a new client

handle_call({connect, ClientId, ClientInfo}, History) ->
	NewHistory = sdd_history:append(History, connect, {ClientId, ClientInfo}),
	{ok, NewHistory}.

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
	State#state{badges = [Badge | State#state.badges]};

%% Set new client

realize_event(State, connect, {ClientId, _ClientInfo}) ->
	State#state{current_client = ClientId}.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(init_peter, init({"Peter", "secret"})).

init_createsNewPlayerWithNameAndSecret_test() ->
	{ok, InitialHistory} = ?init_peter,
	State = sdd_history:state(InitialHistory),
	?assertEqual(State#state.name, "Peter"),
	?assertEqual(State#state.secret, "secret"),
	?assertEqual(State#state.points, 0),

	Past = sdd_history:past(InitialHistory),
	?assertMatch([{_Time, register, {"Peter", "secret"}}], Past).

-define(assert_state_field_equals(History, Field, ExpectedValue),
	State = sdd_history:state(History),
	?assertEqual(State#state.Field, ExpectedValue)
).

-define(assert_past_matches(History, ExpectedMatch),
	Past = sdd_history:past(History),
	?assertMatch(ExpectedMatch, Past)
).

-define(meck_sdd_game_join,
	meck:new(sdd_game),
	meck:expect(sdd_game, join, fun
		(_, "GoodGame", _) -> ok;
		(_, "BadGame", _) -> nope
	end)
).

join_notifiesGameWeWantToJoinAndSavesGameAsCurrentGameIfSuccessful_test() ->
	?meck_sdd_game_join,

	{ok, InitialHistory} = ?init_peter,

	{noreply, HistoryAfterBadJoin} = handle_cast({join, {"BadGame", random}}, InitialHistory),
	?assertEqual(HistoryAfterBadJoin, InitialHistory),

	?assert(meck:called(sdd_game, join, ["Peter", "BadGame", random])),

	{noreply, HistoryAfterGoodJoin} = handle_cast({join, {"GoodGame", invite}}, InitialHistory),

	?assert(meck:called(sdd_game, join, ["Peter", "GoodGame", invite])),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),

	?assert_state_field_equals(HistoryAfterGoodJoin, current_game, "GoodGame"),
	?assert_past_matches(HistoryAfterGoodJoin, [{_Time, join, {"GoodGame", invite}} | _ ]).

-define(init_peter_and_join_good_game,
	fun () ->
		{ok, InitialHistory} = ?init_peter,
		handle_cast({join, {"GoodGame", invite}}, InitialHistory)
	end ()
).

leave_resetsCurrentGame_test() ->
	?meck_sdd_game_join,
	{noreply, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

	meck:expect(sdd_game, leave, fun
		("Peter", "GoodGame", timeout) -> ok
	end),

	{noreply, HistoryAfterLeaving} = handle_cast({leave, timeout}, HistoryAfterGoodJoin),
	
	?assert(meck:called(sdd_game, leave, ["Peter", "GoodGame", timeout])),
	meck:unload(sdd_game),
	
	?assert_state_field_equals(HistoryAfterLeaving, current_game, undefined),
	?assert_past_matches(HistoryAfterLeaving, [{_Time, leave, timeout} | _ ]).

leave_notifiesGameThatWeLeft_test() ->	
	?meck_sdd_game_join,
	{noreply, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

	meck:expect(sdd_game, leave, fun
		("Peter", "GoodGame", timeout) -> ok
	end),

	{noreply, _} = handle_cast({leave, timeout}, HistoryAfterGoodJoin),

	?assert(meck:called(sdd_game, leave, '_')),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

handle_game_event_continuesListeningOnlyIfEventWasFromCurrentGame_test() ->
	?meck_sdd_game_join,
	{noreply, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

	meck:unload(sdd_game),

	?assertMatch(
		{continue_listening, _NewHistory},
		handle_call({game_event, "GoodGame", some_event, some_data}, HistoryAfterGoodJoin)
	),
	?assertMatch(
		{wrong_game, _NewHistory},
		handle_call({game_event, "OtherGame", some_event, some_data}, HistoryAfterGoodJoin)
	).

handle_game_event_redirectsToCurrentClientIfExistsAndIfEventWasFromCurrentGame_test() ->
	?meck_sdd_game_join,
	{noreply, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

	meck:unload(sdd_game),

	%% Nothing should happen when event comes from wrong game, otherwise undef will be thrown here
	handle_call({game_event, "BadGame", some_event, some_data}, HistoryAfterGoodJoin),

	%% Nothing should happen with no client, otherwise undef will be thrown here
	handle_call({game_event, "GoodGame", some_event, some_data}, HistoryAfterGoodJoin),

	meck:new(sdd_client),
	meck:expect(sdd_client, handle_event, fun
		("ClientA", game_event, "GoodGame", some_event, some_data) -> ok
	end),

	handle_call({game_event, "GoodGame", some_event, some_data}, HistoryAfterGoodJoin),

	{ok, HistoryAfterConnect} = handle_call({connect, "ClientA", "ClientAInfo"}, HistoryAfterGoodJoin),
	handle_call({game_event, "GoodGame", some_event, some_data}, HistoryAfterConnect),
	?assert(meck:called(sdd_client, handle_event, ["ClientA", game_event, "GoodGame", some_event, some_data])),
	?assert(meck:validate(sdd_client)),
	meck:unload(sdd_client).


handle_game_event_guess_SavesOwnPositiveGuessResultAndIncreasesPoints_test() ->
	?meck_sdd_game_join,
	{noreply, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

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
	{ok, InitialHistory} = ?init_peter,

	Badge1 = {"Good Test Subject", "For being an important part of these unit tests"},
	Badge2 = {"Good Person", "For having a beautiful personality"},

	{noreply, HistoryAfterGettingFirstBadge} = handle_call({get_badge, Badge1}, InitialHistory),
	{noreply, HistoryAfterGettingSecondBadge} = handle_call({get_badge, Badge2}, HistoryAfterGettingFirstBadge),
	
	State = sdd_history:state(HistoryAfterGettingSecondBadge),
	?assertEqual(State#state.badges, [Badge2, Badge1]),

	Past = sdd_history:past(HistoryAfterGettingSecondBadge),
	?assertMatch([{_Time2, get_badge, Badge2}, {_Time1, get_badge, Badge1} | _ ], Past).

connect_setsNewClient_test() ->
	{ok, InitialHistory} = ?init_peter,
	{ok, HistoryAfterConnect} = handle_call({connect, "ClientA", "ClientAInfo"}, InitialHistory),

	State = sdd_history:state(HistoryAfterConnect),
	?assertEqual(State#state.current_client, "ClientA"),

	Past = sdd_history:past(HistoryAfterConnect),
	?assertMatch([{_Time, connect, {"ClientA", "ClientAInfo"}} | _ ], Past).

-endif.