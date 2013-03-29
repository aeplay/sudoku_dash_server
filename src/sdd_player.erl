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
-export([start_link/2, do/3, handle_game_event/4, register/3, authenticate/2, connect/3]).

%% Records
-record(state, {
	id,
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

start_link(PlayerId, History) ->
	gen_server:start_link({global, {player, PlayerId}}, ?MODULE, [History], []).

do(PlayerId, Action, Args) ->
	gen_server:cast(PlayerId, {Action, Args}).

handle_game_event(PlayerId, GameId, EventType, EventData) ->
	try gen_server:call(PlayerId, {game_event, GameId, EventType, EventData}, 100) of
		Reply -> Reply
	catch
		Error -> Error
	end.

register(PlayerId, Name, Secret) ->
	case sdd_history:persisted_state(player_history, PlayerId) of
		doesnt_exist ->
			InitialHistory = sdd_history:new(fun realize_event/3),
			History = sdd_history:append(InitialHistory, register, {PlayerId, Name, Secret}),
			% TODO start player with history here
			ok;
		_Exists -> already_exists
	end.

authenticate(PlayerId, Secret) ->
	case sdd_history:persisted_state(player_history, PlayerId) of
		#state{secret = Secret} -> true;
		_Else -> false
	end.

connect(PlayerId, ClientId, ClientInfo) ->
	case erlang:is_process_alive(PlayerId) of
		true -> do_nothing;
		false ->
			History = sdd_history:load_persisted(player_history, PlayerId, fun realize_event/3)
			% TODO start player with history here
	end,
	gen_server:cast(PlayerId, {connect, ClientId, ClientInfo}).

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Creates a new player process with the given history

init(InitialHistory) ->
	{ok, InitialHistory}.

%% ------------------------------------------------------------------------------------- %%
%% Connects a new client

handle_cast({connect, ClientId, ClientInfo}, History) ->
	ListenerFunction = fun
		(state, PlayerState) ->
			sdd_client:sync_player_state(
				ClientId,
				PlayerState#state.points,
				PlayerState#state.badges,
				PlayerState#state.current_game
			);
		(event, {_Time, EventType, EventData}) -> sdd_client:handle_player_event(ClientId, EventType, EventData)
	end,
	HistoryWithListener = sdd_history:add_listener(History, ListenerFunction, tell_state),
	NewHistory = sdd_history:append(HistoryWithListener, connect, {ClientId, ClientInfo}),
	{noreply, NewHistory};

%% Leaves the current game, for a reason, and notifies the game

handle_cast({leave, Reason}, History) ->
	State = sdd_history:state(History),
	sdd_game:leave(State#state.id, State#state.current_game, Reason),
	NewHistory = sdd_history:append(History, leave, Reason),
	{noreply, NewHistory};

%% Increases a player's points by a given amount

handle_cast({get_points, Increase}, History) ->	
	NewHistory = sdd_history:append(History, get_points, Increase),
	{noreply, NewHistory};

%% Adds a badge

handle_cast({get_badge, Badge}, History) ->
	NewHistory = sdd_history:append(History, get_badge, Badge),
	{noreply, NewHistory}.

%% ------------------------------------------------------------------------------------- %%
%% Tries to join a game, coming from a source

handle_call({join, {GameId, Source}}, _From, History) ->
	State = sdd_history:state(History),
	case sdd_game:join(State#state.id, GameId, Source) of
		ok ->
			NewHistory = sdd_history:append(History, join, {GameId, Source}),
			{reply, ok, NewHistory};
		Error ->
			{reply, Error, History}
	end;

%% Returns continue_listening for events from our current game
%% and redirects them to the client

handle_call({game_event, GameId, EventType, EventData}, _From, History) ->
	State = sdd_history:state(History),
	CurrentGame = State#state.current_game,
	case GameId of
		CurrentGame ->
			case State#state.current_client of
				undefined -> do_nothing;
				ClientId -> sdd_client:handle_game_event(ClientId, GameId, EventType, EventData)
			end,
			{continue_listening, History};
		_WrongGame ->
			{wrong_game, History}
	end.

%%% =================================================================================== %%%
%%% HISTORY CALLBACKS                                                                   %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Create initial state

realize_event(_EmptyState, register, {Id, Name, Secret}) ->
	#state{id = Id, name = Name, secret = Secret, points = 0};

%% Change current game

realize_event(State, join, {GameId, _Source}) ->
	State#state{current_game = GameId};

%% Reset current game

realize_event(State, leave, _Reason) ->
	State#state{current_game = undefined};

%% Add a badge

realize_event(State, get_badge, Badge) ->
	State#state{badges = [Badge | State#state.badges]};

%% Get points

realize_event(State, get_points, Increase) ->
	State#state{points = State#state.points + Increase};

%% Set new client

realize_event(State, connect, {ClientId, _ClientInfo}) ->
	State#state{current_client = ClientId}.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("sdd_history_test_macros.hrl").


-define(init_peter,
	sdd_history:append(sdd_history:new(fun realize_event/3), register, {"Peter", "Name", "secret"})
).

init_createsPlayerProcessWithAHistory_test() ->
	{ok, InitialHistory} = init(dummy_history),
	?assertEqual(InitialHistory, dummy_history).

-define(meck_sdd_game_join,
	meck:new(sdd_game),
	meck:expect(sdd_game, join, fun
		(_, "GoodGame", _) -> ok;
		(_, "BadGame", _) -> nope
	end)
).

join_notifiesGameWeWantToJoinAndSavesGameAsCurrentGameIfSuccessful_test() ->
	?meck_sdd_game_join,

	InitialHistory = ?init_peter,

	{reply, nope, HistoryAfterBadJoin} = handle_call({join, {"BadGame", random}}, from, InitialHistory),
	?assertEqual(HistoryAfterBadJoin, InitialHistory),

	?assert(meck:called(sdd_game, join, ["Peter", "BadGame", random])),

	{reply, ok, HistoryAfterGoodJoin} = handle_call({join, {"GoodGame", invite}}, from, InitialHistory),

	?assert(meck:called(sdd_game, join, ["Peter", "GoodGame", invite])),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),

	?history_assert_state_field_equals(HistoryAfterGoodJoin, current_game, "GoodGame"),
	?history_assert_past_matches(HistoryAfterGoodJoin, [{_Time, join, {"GoodGame", invite}} | _ ]).

-define(init_peter_and_join_good_game,
	fun () ->
		InitialHistory = ?init_peter,
		handle_call({join, {"GoodGame", invite}}, from, InitialHistory)
	end ()
).

leave_resetsCurrentGame_test() ->
	?meck_sdd_game_join,
	{reply, ok, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

	meck:expect(sdd_game, leave, fun
		("Peter", "GoodGame", timeout) -> ok
	end),

	{noreply, HistoryAfterLeaving} = handle_cast({leave, timeout}, HistoryAfterGoodJoin),

	?assert(meck:called(sdd_game, leave, ["Peter", "GoodGame", timeout])),
	meck:unload(sdd_game),
	
	?history_assert_state_field_equals(HistoryAfterLeaving, current_game, undefined),
	?history_assert_past_matches(HistoryAfterLeaving, [{_Time, leave, timeout} | _ ]).

leave_notifiesGameThatWeLeft_test() ->	
	?meck_sdd_game_join,
	{reply, ok, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

	meck:expect(sdd_game, leave, fun
		("Peter", "GoodGame", timeout) -> ok
	end),

	{noreply, _} = handle_cast({leave, timeout}, HistoryAfterGoodJoin),

	?assert(meck:called(sdd_game, leave, '_')),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

handle_game_event_continuesListeningOnlyIfEventWasFromCurrentGame_test() ->
	?meck_sdd_game_join,
	{reply, ok, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

	meck:unload(sdd_game),

	?assertMatch(
		{continue_listening, _NewHistory},
		handle_call({game_event, "GoodGame", some_event, some_data}, from, HistoryAfterGoodJoin)
	),
	?assertMatch(
		{wrong_game, _NewHistory},
		handle_call({game_event, "OtherGame", some_event, some_data}, from, HistoryAfterGoodJoin)
	).

-define(meck_sdd_client, 
	meck:new(sdd_client),
	meck:expect(sdd_client, handle_game_event, fun
		(_ClientId, _GameId, _EventType, _EventData) -> ok
	end),
	meck:expect(sdd_client, handle_player_event, fun
		(_ClientId, _EventType, _EventData) -> ok
	end),
	meck:expect(sdd_client, sync_player_state, fun
		(_ClientId, _Points, _Badges, _CurrentGame) -> ok
	end)
).	

handle_game_event_redirectsToCurrentClientIfExistsAndIfEventWasFromCurrentGame_test() ->
	?meck_sdd_game_join,
	{reply, ok, HistoryAfterGoodJoin} = ?init_peter_and_join_good_game,

	meck:unload(sdd_game),

	%% Nothing should happen when event comes from wrong game, otherwise undef will be thrown here
	handle_call({game_event, "BadGame", some_event, some_data}, from, HistoryAfterGoodJoin),

	%% Nothing should happen with no client, otherwise undef will be thrown here
	handle_call({game_event, "GoodGame", some_event, some_data}, from, HistoryAfterGoodJoin),

	?meck_sdd_client,

	{noreply, HistoryAfterConnect} = handle_cast({connect, "ClientA", "ClientAInfo"}, HistoryAfterGoodJoin),
	handle_call({game_event, "GoodGame", some_event, some_data}, from, HistoryAfterConnect),
	?assert(meck:called(sdd_client, handle_game_event, ["ClientA", "GoodGame", some_event, some_data])),
	?assert(meck:validate(sdd_client)),
	meck:unload(sdd_client).

get_points_addsPoints_test() ->
	InitialHistory = ?init_peter,

	{noreply, HistoryAfterGettingPoints} = handle_cast({get_points, 3}, InitialHistory),

	?history_assert_state_field_equals(HistoryAfterGettingPoints, points, 3),
	?history_assert_past_matches(HistoryAfterGettingPoints, [{_Time, get_points, 3} | _ ]).

get_badge_addsABadge_test() ->
	InitialHistory = ?init_peter,

	Badge1 = {"Good Test Subject", "For being an important part of these unit tests"},
	Badge2 = {"Good Person", "For having a beautiful personality"},

	{noreply, HistoryAfterGettingFirstBadge} = handle_cast({get_badge, Badge1}, InitialHistory),
	{noreply, HistoryAfterGettingSecondBadge} = handle_cast({get_badge, Badge2}, HistoryAfterGettingFirstBadge),
	
	?history_assert_state_field_equals(HistoryAfterGettingSecondBadge, badges, [Badge2, Badge1]),
	?history_assert_past_matches(HistoryAfterGettingSecondBadge, [{_Time2, get_badge, Badge2}, {_Time1, get_badge, Badge1} | _ ]).

connect_setsNewClientAndMakesClientAListenerOfPlayerHistory_test() ->
	InitialHistory = ?init_peter,

	?meck_sdd_client,

	% make sure client gets player state without secret field
	State = sdd_history:state(InitialHistory),

	{noreply, HistoryAfterConnect} = handle_cast({connect, "ClientA", "ClientAInfo"}, InitialHistory),

	?assert(meck:called(sdd_client, sync_player_state, ["ClientA", State#state.points, State#state.badges, State#state.current_game])),

	?assert(meck:called(sdd_client, handle_player_event, ["ClientA", connect, {"ClientA", "ClientAInfo"}])),
	?assert(meck:validate(sdd_client)),
	meck:unload(sdd_client),

	?history_assert_state_field_equals(HistoryAfterConnect, current_client, "ClientA"),
	?history_assert_past_matches(HistoryAfterConnect, [{_Time, connect, {"ClientA", "ClientAInfo"}} | _ ]).

-endif.