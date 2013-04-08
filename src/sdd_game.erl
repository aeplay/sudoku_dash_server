%%% =================================================================================== %%%
%%% Sudoku Dash: Game                                                                   %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This module manages one game of sudoku dash. Its job is to connect its history,     %%%
%%% which is holding its state, to the participating players. It does so by             %%%
%%% registering players as listeners of the history and offers actions that players     %%%
%%% can take, which will generate new history events.                                   %%%
%%% =================================================================================== %%%

-module(sdd_game).

%% API
-export([start_link/1, join/4, do/4]).

%% GEN_SERVER
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

%% Records
-record(state, {
	id,
	board,
	candidates,
	complete = false,
	n_players = 0
}).

%% Config
-define(CANDIDATE_SOPHISTICATION, 3).

-ifdef(TEST).
-define(TIMEOUT_AFTER_COMPLETE, 10).
-else.
-define(TIMEOUT_AFTER_COMPLETE, 10000).
-endif.


%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

start_link(Id) ->
	gen_server:start_link({global, {game, Id}}, ?MODULE, Id, []).

join(GameId, PlayerId, PlayerInfo, Source) ->
	gen_server:call({global, {game, GameId}}, {join, PlayerId, PlayerInfo, Source}).

do(GameId, PlayerId, Action, Args) ->
	gen_server:cast({global, {game, GameId}}, {Action, PlayerId, Args}).

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Initializes the history, and assigns the realizer function to it.
%% Generates the intial sudoku board saves that as the first event

init(Id) ->
	EmptyHistory = sdd_history:new(fun realize_event/3),
	InitialBoard = sdd_logic:generate_sudoku(),
	InitialHistory = sdd_history:append(EmptyHistory, start, {Id, InitialBoard}),
	{ok, InitialHistory}.

%% ------------------------------------------------------------------------------------- %%
%% Adds a player to the game (if it's not full), by creating an listener function

handle_call({join, PlayerId, PlayerInfo, Source}, _From, History) ->
	State = sdd_history:state(History),
	ListenerFunction = fun(event, {Time, EventType, EventData}) ->
		sdd_player:handle_game_event(PlayerId, State#state.id, Time, EventType, EventData)
	end,
	HistoryWithNewListener = sdd_history:add_listener(History, ListenerFunction, replay_past),
	NewHistory = sdd_history:append(HistoryWithNewListener, join, {PlayerId, PlayerInfo, Source}),
	{reply, ok, NewHistory}.

%% ------------------------------------------------------------------------------------- %%
%% Adds a chat message to the history

handle_cast({chat, PlayerId, Message}, History) ->
	NewHistory = sdd_history:append(History, chat, {PlayerId, Message}),
	{noreply, NewHistory};

%% Notifies other players that a player left

handle_cast({leave, PlayerId, Reason}, History) ->
	NewHistory = sdd_history:append(History, leave, {PlayerId, Reason}),
	{noreply, NewHistory};

%% Handles a guess, looks if it was right and creates an according event
%% If the game is now complete, starts timeout after which it is stopped

handle_cast({guess, PlayerId, {Position, Number}}, History) ->
	case sdd_history:state(History) of
		#state{complete = true} ->
			{noreply, History};
		#state{board = Board, candidates = Candidates, complete = false} ->
			Result = sdd_logic:check_guess(Position, Number, Board, Candidates),
			GuessEventData = {PlayerId, Position, Number, Result},
			NewHistory = sdd_history:append(History, guess, GuessEventData),
			NewState = sdd_history:state(NewHistory),
			case NewState of
				#state{complete = true} ->
					sdd_games_manager:remove_game(NewState#state.id),
					erlang:send_after(?TIMEOUT_AFTER_COMPLETE, self(), stop_complete),
					{noreply, NewHistory};
				#state{complete = false} ->
					{noreply, NewHistory}
			end
	end.

%% ------------------------------------------------------------------------------------- %%
%% Stops a game if it is complete

handle_info(stop_complete, History) ->
	{stop, normal, History}.

terminate(_Reason, History) ->
	State = sdd_history:state(History),
	sdd_history:save_persisted(game_history, State#state.id, History),
	ok.

%% ------------------------------------------------------------------------------------- %%
%% Rest of gen_server calls

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%% =================================================================================== %%%
%%% HISTORY CALLBACKS                                                                   %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Uses the initial sudoku board to create the initial state, including candidates

realize_event(_EmptyState, start, {Id, InitialBoard}) ->
	InitialCandidates = sdd_logic:calculate_candidates(InitialBoard, ?CANDIDATE_SOPHISTICATION),
	#state{id = Id, board = InitialBoard, candidates = InitialCandidates};

%% Update the board and candidates after a good guess, check if it is complete

realize_event(State, guess, {PlayerId, Position, Number, {good}}) ->
	sdd_player:do(PlayerId, get_points, 1),
	NewBoard = array:set(Position, Number, State#state.board),
	NewCandidates = sdd_logic:calculate_candidates(NewBoard, ?CANDIDATE_SOPHISTICATION),
	Complete = sdd_logic:is_complete(NewBoard),
	State#state{board = NewBoard, candidates = NewCandidates, complete = Complete};

realize_event(State, guess, {PlayerId, _Position, _Number, {bad, Conflicts}}) ->
	sdd_player:do(PlayerId, get_points, -2*length(Conflicts)),
	State;

realize_event(State, guess, {PlayerId, _Position, _Number, {ambigous, _Reason}}) ->
	sdd_player:do(PlayerId, get_points, -3),
	State;

%% ignore guess if field is already filled

realize_event(State, guess, {_PlayerId, _Position, _Number, {already_filled}}) ->
	State;

%% Does not change state for all other used events

realize_event(State, chat, _) -> State;
realize_event(State, join, _) -> State;
realize_event(State, leave, _) -> State.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("sdd_history_test_macros.hrl").

-define(meck_sdd_player,
	meck:new(sdd_player),
	meck:expect(sdd_player, handle_game_event, fun
		(_, _, _, _, _) -> continue_listening
	end),
	meck:expect(sdd_player, do, fun
		(_, _, _) -> ok
	end)
).

-define(example_board, 
	array:from_list(
		[4,1,6,5,2,0,8,9,3,
		 5,9,2,8,3,6,1,4,7,
		 8,7,3,4,9,1,2,6,5,
		 0,4,8,2,6,5,3,7,9,
		 6,5,7,3,1,9,4,8,2,
		 2,3,9,7,8,4,6,5,1,
		 3,6,1,9,5,8,7,2,4,
		 7,8,5,1,4,2,9,0,6,
		 9,2,4,6,7,3,5,1,8]
	)
).

init_returnsHistoryWithInitialBoardAndCandidates_test() ->
	{ok, InitialHistory} = init("GameId"),
	State = sdd_history:state(InitialHistory),
	?assertNot(State#state.board =:= undefined),
	?assertNot(State#state.candidates =:= undefined),
	?assertEqual(false, State#state.complete),
	?assertEqual("GameId", State#state.id),

	Board = State#state.board,
	?history_assert_past_matches(InitialHistory, [{_Time, start, {"GameId", Board}}]).

chat_addsChatMessageToHistory_test() ->
	DummyHistory = sdd_history:new(fun realize_event/3),
	{noreply, HistoryAfterChat} = handle_cast({chat, "Peter", "Hello"}, DummyHistory),
	?history_assert_past_matches(HistoryAfterChat, [{_Time, chat, {"Peter", "Hello"}}]).

join_createsJoinEvent_test() ->
	DummyHistory = sdd_history:new(fun realize_event/3),
	InitialHistory = sdd_history:append(DummyHistory, start, {"GameId", ?example_board}),
	
	?meck_sdd_player,

	{reply, ok, HistoryAfterJoin} = handle_call({join, "Peter", "PeterInfo", random}, from, InitialHistory),
	?history_assert_past_matches(HistoryAfterJoin, [{_Time, join, {"Peter", "PeterInfo", random}} | _]),

	meck:unload(sdd_player).

join_addsPlayerListenerFunctionToHistory_test() ->
	DummyHistory = sdd_history:new(fun realize_event/3),
	InitialHistory = sdd_history:append(DummyHistory, start, {"GameId", ?example_board}),
	
	?meck_sdd_player,

	{reply, ok, _HistoryAfterJoin} = handle_call({join, "Peter", "PeterInfo", random}, from, InitialHistory),
	?assert(meck:called(sdd_player, handle_game_event, ["Peter", "GameId", '_', start, '_'])),
	?assert(meck:called(sdd_player, handle_game_event, ["Peter", "GameId", '_', join, {"Peter", "PeterInfo", random}])),

	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player).

leave_createsLeaveEvent_test() ->
	DummyHistory = sdd_history:new(fun realize_event/3),
	InitialHistory = sdd_history:append(DummyHistory, start, {"GameId", ?example_board}),

	?meck_sdd_player,

	{reply, ok, HistoryAfterJoin} = handle_call({join, "Peter", "PeterInfo", random}, from, InitialHistory),
	{noreply, HistoryAfterLeave} = handle_cast({leave, "Peter", fell_asleep}, HistoryAfterJoin),
	meck:unload(sdd_player),

	?history_assert_past_matches(HistoryAfterLeave, [{_Time, leave, {"Peter", fell_asleep}} | _ ]).

guess_updatesBoardOnCorrectGuessAndGivesPlayerAPoint_test() ->
	InitialBoard = array:from_list(
		[4,1,6,5,2,0,8,9,3,
		 5,9,2,8,3,6,1,4,7,
		 8,7,3,4,9,1,2,6,5,
		 0,4,8,2,6,5,3,7,9,
		 6,5,7,3,1,9,4,8,2,
		 2,3,9,7,8,4,6,5,1,
		 3,6,1,9,5,8,7,2,4,
		 7,8,5,1,4,2,9,0,6,
		 9,2,4,6,7,3,5,1,8]
	),
	DummyHistory = sdd_history:new(fun realize_event/3),
	InitialHistory = sdd_history:append(DummyHistory, start, {"GameId", InitialBoard}),

	?meck_sdd_player,

	{noreply, HistoryAfterGuess} = handle_cast({guess, "Peter", {27, 1}}, InitialHistory),

	?assert(meck:called(sdd_player, do, ["Peter", get_points, 1])),
	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player),
	
	State = sdd_history:state(HistoryAfterGuess),
	?assertEqual(1, array:get(27, State#state.board)),

	?history_assert_past_matches(HistoryAfterGuess, [{_Time, guess, {"Peter", 27, 1, {good}}} | _ ]).

guess_addsGuessToHistoryAndPunishes2PerConflictOnInvalidGuess_test() ->
	InitialBoard = array:from_list(
		[4,1,6,5,2,0,8,9,3,
		 5,9,2,8,3,6,1,4,7,
		 8,7,3,4,9,1,2,6,5,
		 0,4,8,2,6,5,3,7,9,
		 6,5,7,3,1,9,4,8,2,
		 2,3,9,7,8,4,6,5,1,
		 3,6,1,9,5,8,7,2,4,
		 7,8,5,1,4,2,9,0,6,
		 9,2,4,6,7,3,5,1,8]
	),
	DummyHistory = sdd_history:new(fun realize_event/3),
	InitialHistory = sdd_history:append(DummyHistory, start, {"GameId", InitialBoard}),

	?meck_sdd_player,

	{noreply, HistoryAfterGuess} = handle_cast({guess, "Peter", {27, 3}}, InitialHistory),

	?assert(meck:called(sdd_player, do, ["Peter", get_points, -6])),
	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player),

	?history_assert_past_matches(HistoryAfterGuess, [{_Time, guess, {"Peter", 27, 3, {bad, _Conflicts}}} | _ ]),
	?history_assert_states_equal(InitialHistory, HistoryAfterGuess).

guess_punishes3OnAmbigous_test() ->
	?meck_sdd_player,

	StateAfterGuess = realize_event(dummy_state, guess, {"Peter", 33, 7, {ambigous, because_i_say_so}}),
	
	?assert(meck:called(sdd_player, do, ["Peter", get_points, -3])),
	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player),

	?assertEqual(dummy_state, StateAfterGuess).


guess_canHandleAlreadyFilledResult_test() ->	
	?assertEqual(dummy_state, realize_event(dummy_state, guess, {"Peter", 33, 7, {already_filled}})).

guess_markGameAsCompleteAndTimeoutIfComplete_test() ->
	InitialBoard = array:from_list(
		[4,1,6,5,2,7,8,9,3,
		 5,9,2,8,3,6,1,4,7,
		 8,7,3,4,9,1,2,6,5,
		 0,4,8,2,6,5,3,7,9,
		 6,5,7,3,1,9,4,8,2,
		 2,3,9,7,8,4,6,5,1,
		 3,6,1,9,5,8,7,2,4,
		 7,8,5,1,4,2,9,3,6,
		 9,2,4,6,7,3,5,1,8]
	),

	DummyHistory = sdd_history:new(fun realize_event/3),
	InitialHistory = sdd_history:append(DummyHistory, start, {"GameId", InitialBoard}),

	?meck_sdd_player,

	{noreply, HistoryAfterGuess} = handle_cast({guess, "Peter", {27, 1}}, InitialHistory),

	meck:unload(sdd_player),

	?history_assert_state_field_equals(HistoryAfterGuess, complete, true),

	receive
		stop_complete ->
			?assert(true)
	after
		?TIMEOUT_AFTER_COMPLETE + 100 ->
			?assert(false)
	end.

guess_ignoresGuessAfterComplete_test() ->
	InitialBoard = array:from_list(
		[4,1,6,5,2,7,8,9,3,
		 5,9,2,8,3,6,1,4,7,
		 8,7,3,4,9,1,2,6,5,
		 0,4,8,2,6,5,3,7,9,
		 6,5,7,3,1,9,4,8,2,
		 2,3,9,7,8,4,6,5,1,
		 3,6,1,9,5,8,7,2,4,
		 7,8,5,1,4,2,9,3,6,
		 9,2,4,6,7,3,5,1,8]
	),

	DummyHistory = sdd_history:new(fun realize_event/3),
	InitialHistory = sdd_history:append(DummyHistory, start, {"GameId", InitialBoard}),

	?meck_sdd_player,

	{noreply, HistoryAfterCompletingGuess} = handle_cast({guess, "Peter", {27, 1}}, InitialHistory),

	meck:unload(sdd_player),

	{noreply, HistoryAfterNewGuess} = handle_cast({guess, "Peter", {27, 1}}, HistoryAfterCompletingGuess),
	?assertEqual(HistoryAfterCompletingGuess, HistoryAfterNewGuess).

game_stopsWhenRecievingCompleteTimeout_test() ->
	?assertEqual({stop, normal, dummy_history}, handle_info(stop_complete, dummy_history)).

terminate_persistsGameHistory_test() ->
	meck:new(sdd_history),
	meck:expect(sdd_history, state, fun(dummy_history) -> #state{id = "GameId"} end),
	meck:expect(sdd_history, save_persisted, fun(_, _, _) -> {atomic, ok} end),

	terminate("Reason", dummy_history),

	?assert(meck:called(sdd_history, save_persisted, [game_history, "GameId", dummy_history])),
	?assert(meck:validate(sdd_history)),
	meck:unload(sdd_history).

code_change_doesNothingRightNow_test() ->
	?assertEqual({ok, dummy_history}, code_change(0, dummy_history, [])).

-endif.