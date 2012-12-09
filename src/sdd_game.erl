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

%% Records
-record(state, {
	board,
	candidates,
	complete = false
}).

%% Config
-define(CANDIDATE_SOPHISTICATION, 3).

-ifdef(TEST).
-define(TIMEOUT_AFTER_COMPLETE, 10).
-else.
-define(TIMEOUT_AFTER_COMPLETE, 10000).
-endif.

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Initializes the history, and assigns the realizer function to it.
%% Generates the intial sudoku board saves that as the first event

init(_Opts) ->
	EmptyHistory = sdd_history:new(fun realize_event/3),
	InitialBoard = sdd_logic:generate_sudoku(),
	InitialHistory = sdd_history:append(EmptyHistory, start, InitialBoard),
	{ok, InitialHistory}.

%% ------------------------------------------------------------------------------------- %%
%% Adds a chat message to the history

handle_cast({chat, PlayerId, Message}, History) ->
	NewHistory = sdd_history:append(History, chat, {PlayerId, Message}),
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
			case sdd_history:state(NewHistory) of
				#state{complete = true} ->
					erlang:send_after(?TIMEOUT_AFTER_COMPLETE, self(), stop_complete),
					{noreply, NewHistory};
				#state{complete = false} ->
					{noreply, NewHistory}
			end
	end.

%% ------------------------------------------------------------------------------------- %%
%% Stops a game if it is complete

handle_info(stop_complete, History) ->
	{stop, complete, History}.

%%% =================================================================================== %%%
%%% HISTORY CALLBACKS                                                                   %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Uses the initial sudoku board to create the initial state, including candidates

realize_event(_EmptyState, start, InitialBoard) ->
	InitialCandidates = sdd_logic:calculate_candidates(InitialBoard, ?CANDIDATE_SOPHISTICATION),
	#state{board = InitialBoard, candidates = InitialCandidates};

%% Update the board and candidates after a good guess, check if it is complete

realize_event(State, guess, {_PlayerId, Position, Number, {good}}) ->
	NewBoard = array:set(Position, Number, State#state.board),
	NewCandidates = sdd_logic:calculate_candidates(NewBoard, ?CANDIDATE_SOPHISTICATION),
	Complete = sdd_logic:is_complete(NewBoard),
	State#state{board = NewBoard, candidates = NewCandidates, complete = Complete};

%% Does not change state for guesses that are not good

realize_event(State, guess, {_, _, _, _}) -> State;

%% Does not change state for all other used events

realize_event(State, chat, _) -> State.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

init_returnsHistoryWithInitialBoardAndCandidates_test() ->
	{ok, InitialHistory} = init(no_options),
	State = sdd_history:state(InitialHistory),
	?assertNot(State#state.board =:= undefined),
	?assertNot(State#state.candidates =:= undefined),
	?assertEqual(false, State#state.complete),

	Board = State#state.board,
	Past = sdd_history:past(InitialHistory),
	?assertMatch([{_Time, start, Board}], Past).

chat_addsChatMessageToHistory_test() ->
	DummyHistory = sdd_history:new(fun realize_event/3),
	{noreply, HistoryAfterChat} = handle_cast({chat, "Peter", "Hello"}, DummyHistory),
	Past = sdd_history:past(HistoryAfterChat),
	?assertMatch([{_Time, chat, {"Peter", "Hello"}}], Past).

guess_updatesBoardOnCorrectGuess_test() ->
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
	InitialHistory = sdd_history:append(DummyHistory, start, InitialBoard),
	{noreply, HistoryAfterGuess} = handle_cast({guess, "Peter", {27, 1}}, InitialHistory),
	
	State = sdd_history:state(HistoryAfterGuess),
	?assertEqual(1, array:get(27, State#state.board)),

	[LastEvent|_Rest] = sdd_history:past(HistoryAfterGuess),
	?assertMatch({_Time, guess, {"Peter", 27, 1, {good}}}, LastEvent).

guess_addsGuessToHistoryButLeavesBoardAloneOnInvalidGuess_test() ->
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
	InitialHistory = sdd_history:append(DummyHistory, start, InitialBoard),
	InitialState = sdd_history:state(InitialHistory),
	{noreply, HistoryAfterGuess} = handle_cast({guess, "Peter", {27, 3}}, InitialHistory),

	StateAfterGuess = sdd_history:state(HistoryAfterGuess),
	?assertEqual(StateAfterGuess, InitialState),

	[LastEvent|_Rest] = sdd_history:past(HistoryAfterGuess),
	?assertMatch({_Time, guess, {"Peter", 27, 3, {bad, _Conflicts}}}, LastEvent).

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
	InitialHistory = sdd_history:append(DummyHistory, start, InitialBoard),
	{noreply, HistoryAfterGuess} = handle_cast({guess, "Peter", {27, 1}}, InitialHistory),

	StateAfterGuess = sdd_history:state(HistoryAfterGuess),
	?assertEqual(true, StateAfterGuess#state.complete),

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
	InitialHistory = sdd_history:append(DummyHistory, start, InitialBoard),
	{noreply, HistoryAfterCompletingGuess} = handle_cast({guess, "Peter", {27, 1}}, InitialHistory),

	{noreply, HistoryAfterNewGuess} = handle_cast({guess, "Peter", {27, 1}}, HistoryAfterCompletingGuess),
	?assertEqual(HistoryAfterCompletingGuess, HistoryAfterNewGuess).

game_stopsWhenRecievingCompleteTimeout_test() ->
	?assertEqual({stop, complete, dummy_history}, handle_info(stop_complete, dummy_history)).

-endif.