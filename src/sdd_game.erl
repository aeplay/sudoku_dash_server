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

%%% =================================================================================== %%%
%%% HISTORY CALLBACKS                                                                   %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Uses the initial sudoku board to create the initial state, including candidates

realize_event(_EmptyState, start, InitialBoard) ->
	InitialCandidates = sdd_logic:calculate_candidates(InitialBoard, ?CANDIDATE_SOPHISTICATION),
	#state{board = InitialBoard, candidates = InitialCandidates}.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

init_returnsHistoryWithInitialBoardAndCandidates_test() ->
	{ok, History} = init(no_options),
	State = sdd_history:state(History),
	?assertNot(State#state.board =:= undefined),
	?assertNot(State#state.candidates =:= undefined),
	?assertEqual(false, State#state.complete),

	Board = State#state.board,
	Past = sdd_history:past(History),
	?assertMatch([{_Time, start, Board}], Past).

-endif.