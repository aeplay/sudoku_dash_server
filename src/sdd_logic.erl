%%% =================================================================================== %%%
%%% Sudoku Dash Game: Logic                                                             %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This module offers:                                                                 %%%
%%% - traversion,                                                                       %%%
%%% - inspection of candidates,                                                      %%%
%%% - generation                                                                        %%%
%%% of Sudoku boards.                                                                   %%%
%%% (general knowledge of Sudoku terminology is assumed)                                %%%
%%% =================================================================================== %%%

-module(sdd_logic).

%%% Board Traversion
-export([for_house_of/4, for_nth_house/4, for_houses_of/3]).

%%% Basic Board Inspection
-export([is_valid/3, is_complete/1]).

%%% Possibility Array Operations
-export([candidates_to_list/2]).

%%% Possibility Calculation
-export([calculate_candidates/2]).

%%% Sudoku Generation
-export([generate_sudoku/0]).

%% Guess checking
-export([check_guess/4]).

%% Types and Records
-export_type([board/0, board_position/0, board_candidates/0, sudoku_number/0, guess_result/0]).
-type house() :: row|column|box.
-type house_index() :: 0..8.
-type board() :: array().
-type board_position() :: 0..80.
-type board_candidates() :: array().
-type sudoku_number() :: 1..9.
-type candidate_occurences() :: dict().
-type pair_occurences() :: dict().
-type guess_result() :: {good}|{neutral, Reason}|{bad, Reason}.


%%% =================================================================================== %%%
%%% BOARD TRAVERSION                                                                    %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% for_house_of
%% Applies Fun in a folding fashion to the positions given by the
%% Type of house and a square in that house (at Pos).

-spec for_house_of(
	Type :: house(),
	Pos :: board_position(),
	AccStart :: X,
	Fun :: fun((Pos :: board_position(), AccIn :: X) -> AccOut :: X)
) -> AccFinal :: X.

for_house_of(Type, Pos, AccStart, Fun) ->
	X = Pos rem 9,
	Y = Pos div 9,

	{Start, Offsets} = case Type of
		row ->
			{9*Y, [0,1,2,3,4,5,6,7,8]};
		column ->
			{X, [0,9,18,27,36,45,54,63,72]};
		box ->
			StartX = X - X rem 3,
			StartY = Y - Y rem 3,
			{StartX + 9*StartY, [0,1,2,9,10,11,18,19,20]}
	end,
	
	lists:foldl(Fun, AccStart, [Start + Offset || Offset <- Offsets]).

%% ------------------------------------------------------------------------------------- %%
%% for_nth_house
%% Applies Fun in a folding fashion to the positions given by the
%% nth house of Type.

-spec for_nth_house(
	Type :: house(),
	N :: house_index(),
	AccStart :: X,
	Fun :: fun((Pos :: board_position(), AccIn::X) -> AccOut :: X)
) -> AccFinal :: X.

for_nth_house(Type, N, AccStart, Fun) ->

	StartPos = case Type of
		row -> N*9;
		column -> N;
		box -> 3*N rem 9 + 27*N div 3
	end,

	for_house_of(Type, StartPos, AccStart, Fun).

%% ------------------------------------------------------------------------------------- %%
%% for_houses_of
%% Applies Fun in a folding fashion to the positions given by all houses
%% that the square at Pos is a member of.

-spec for_houses_of(
	Pos :: board_position(),
	AccStart :: X,
	Fun :: fun((Pos :: board_position(), AccIn::X) -> AccOut :: X)
) -> AccFinal :: X.

for_houses_of(Pos, AccStart, Fun) ->
	Result0 = for_house_of(row, Pos, AccStart, Fun),
	Result1 = for_house_of(column, Pos, Result0, Fun),
	for_house_of(box, Pos, Result1, Fun).


%%% =================================================================================== %%%
%%% BASIC BOARD INSPECTION                                                              %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% is_valid
%% Checks whether the given Num would be valid at Pos on the given Board.
%% If not, it also returns a list of conflicting squares.

-spec is_valid(
	Pos :: board_position(),
	Num :: sudoku_number(),
	Board :: board()
) -> {Valid :: boolean(), Conflicts :: []|[board_position()]}.

is_valid(Pos, Num, Board) ->
	for_houses_of(Pos, {true, []}, fun(CurrentPos, Acc = {_Valid, Conflicts}) ->
		case CurrentPos of
			Pos ->
				Acc;
			_SomewhereElse ->
				CurrentNum = array:get(CurrentPos, Board),
				case CurrentNum of
					Num ->
						{false, [CurrentPos|Conflicts]};
					_IsDifferent ->
						Acc
				end
		end
	end).

%% ------------------------------------------------------------------------------------- %%
%% is_complete
%% Checks whether all squares of the given Board are filled out

-spec is_complete(
	Board :: board()
) -> Complete :: boolean().

is_complete(Board) ->
	array:foldl(fun(_I, Num, Acc) -> Acc andalso 0 < Num end, true, Board).


%%% =================================================================================== %%%
%%% POSSIBILITY ARRAY OPERATIONS                                                        %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% has_psb
%% Checks whether the given Number is a possibility at Pos in the Candidates array.

-spec has_psb(
	Pos :: board_position(),
	Num :: sudoku_number(),
	Candidates :: board_candidates()
) -> DoesHave :: boolean().

has_psb(Pos, Num, Candidates) ->
	0 =/= (array:get(Pos, Candidates) band (1 bsl (Num-1))).

%% ------------------------------------------------------------------------------------- %%
%% add_psb
%% Adds the given Num as a possibility to the Candidates array at Pos.

-spec add_psb(
	Pos :: board_position(),
	Num :: sudoku_number(),
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

add_psb(Pos, Num, Candidates) ->
	array:set(Pos, array:get(Pos, Candidates) bor (1 bsl (Num-1)), Candidates).

%% ------------------------------------------------------------------------------------- %%
%% remove_psb
%% Removes the given Num as a possibility from the Candidates array at Pos.

-spec remove_psb(
	Pos :: board_position(),
	Num :: sudoku_number(),
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

remove_psb(Pos, Num, Candidates) ->
	array:set(Pos, array:get(Pos, Candidates) band bnot(1 bsl (Num-1)), Candidates).

%% ------------------------------------------------------------------------------------- %%
%% set_only_psb
%% Sets the given Num as the only possibility in the Candidates array at Pos.

-spec set_only_psb(
	Pos :: board_position(),
	Num :: sudoku_number(),
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

set_only_psb(Pos, Num, Candidates) ->
	array:set(Pos, 1 bsl (Num-1), Candidates).

%% ------------------------------------------------------------------------------------- %%
%% candidates_to_list
%% Returns the candidates at Pos in Candidates as a list

-spec candidates_to_list(
	Pos :: board_position(),
	Candidates :: board_candidates()
) -> AsList :: []|[sudoku_number()].

candidates_to_list(Pos, Candidates) ->
	[X || X <- [1,2,3,4,5,6,7,8,9], has_psb(Pos, X, Candidates)].

%% ------------------------------------------------------------------------------------- %%
%% remove_psb_from_peers_of
%% Removes the Psb from all peers of the square at Pos, except itself

-spec remove_psb_from_peers_of(
	Pos :: board_position(),
	Psb :: sudoku_number(),
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

remove_psb_from_peers_of(Pos, Psb, Candidates) ->
	for_houses_of(Pos, Candidates, fun(CurrentPos, CandidatesAcc) ->
		case CurrentPos of
			Pos ->
				CandidatesAcc;
			_SomewhereElse ->
				remove_psb(CurrentPos, Psb, CandidatesAcc)		
		end
	end).


%%% =================================================================================== %%%
%%% POSSIBILITY CALCULATION                                                             %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% calculate_candidates
%% Calculates the possible Numbers for each square given a partially solved Board,
%% with techniques with the given Sophistication

-spec calculate_candidates(
	Board :: board(),
	Sophistication :: 0..3
) -> Candidates :: board_candidates().

calculate_candidates(Board, Sophistication) ->
	Candidates0 = array:new([{size,81},{fixed,true},{default, (1 bsl 9) - 1}]),

	Candidates1 = remove_naked_candidates(Board, Candidates0),

	Candidates2 = if
		Sophistication > 0 ->
			remove_using_hidden_candidates(Candidates1);
		Sophistication =< 0 ->
			Candidates1
	end,

	Candidates3 = if
		Sophistication > 1 ->
			remove_using_hidden_candidates(remove_using_box_line(Candidates2));
		Sophistication =< 1 ->
			Candidates2
	end,

	_Candidates4 = if
		Sophistication > 2 ->
			remove_using_hidden_candidates(remove_using_pairs(Candidates3));
		Sophistication =< 2 ->
			Candidates3
	end.

	%TODO RETURN ALL STAGES OF CANDIDATES!

%% ------------------------------------------------------------------------------------- %%
%% remove_naked_candidates
%% Looks at the given numbers in Board, sets the candidates there to only the given
%% number and removes that number as a possibility from all its peers.

-spec remove_naked_candidates(
	Board :: board(),
	Candidates :: array()
) -> NewCandidates :: board_candidates().

remove_naked_candidates(Board, Candidates) ->
	lists:foldl(fun(Pos, CandidatesAcc) ->
		Num = array:get(Pos, Board),
		case Num of
			0 ->
				CandidatesAcc;
			_IsGiven ->
				CandidatesAcc1 = set_only_psb(Pos, Num, CandidatesAcc),
				remove_psb_from_peers_of(Pos, Num, CandidatesAcc1)
		end
	end, Candidates, lists:seq(0, 80)).

%% ------------------------------------------------------------------------------------- %%
%% remove_using_hidden_candidates
%% Looks for houses that contain a certain possibility only once. This means it has to be
%% only there and can be removed from the peers in the other houses of that square

-spec remove_using_hidden_candidates(
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

remove_using_hidden_candidates(Candidates) ->
	lists:foldl(fun(N, CandidatesAcc) ->
		Candidates0 = remove_using_hidden_candidates_in_house(row, N, CandidatesAcc),
		Candidates1 = remove_using_hidden_candidates_in_house(column, N, Candidates0),
		remove_using_hidden_candidates_in_house(box, N, Candidates1)
	end, Candidates, lists:seq(0, 8)).

-spec remove_using_hidden_candidates_in_house(
	Type :: house(),
	N :: house_index(),
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

remove_using_hidden_candidates_in_house(Type, N, Candidates) ->
	Occurences0 = candidate_occurences_in_house(Type, N, Candidates),
	OccurencesOnce = dict:filter(fun(_Num, Positions) ->
		length(Positions) =:= 1
	end, Occurences0),

	Candidates1 = dict:fold(fun(Num, [Pos], CandidatesAcc) ->
		CandidatesAcc1 = set_only_psb(Pos, Num, CandidatesAcc),
		remove_psb_from_peers_of(Pos, Num, CandidatesAcc1)
	end, Candidates, OccurencesOnce),

	Candidates1.

%% ------------------------------------------------------------------------------------- %%
%% remove_using_box_line
%% Removes candidates by looking at Box-Line/Line-Box Interactions

-spec remove_using_box_line(
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

remove_using_box_line(Candidates) ->
	lists:foldl(fun(N, CandidatesAcc) ->
		Candidates0 = remove_using_box_line_in_house(row, N, CandidatesAcc),
		Candidates1 = remove_using_box_line_in_house(column, N, Candidates0),
		remove_using_box_line_in_house(box, N, Candidates1)
	end, Candidates, lists:seq(0, 8)).

-spec remove_using_box_line_in_house(
	ThisHouse :: house(),
	N :: house_index(),
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

remove_using_box_line_in_house(ThisHouse, N, Candidates) ->
	Occurences0 = candidate_occurences_in_house(ThisHouse, N, Candidates),
	OccurencesMoreTwice = dict:filter(fun(_Num, Positions) ->
		length(Positions) =:= 2 orelse length(Positions) =:= 3
	end, Occurences0),

	dict:fold(fun(Num, Positions, CandidatesAccI) ->
		case in_same_other_house(ThisHouse, Positions) of
			{true, OtherHouse} ->
				[PosA|_Rest] = Positions,
				for_house_of(OtherHouse, PosA, CandidatesAccI, fun(CurrentPos, CandidatesAccII) ->
					% we need this check in the case of for example
					% a triple of the Psb in a row
					case in_same_house(ThisHouse, [PosA, CurrentPos]) of
						true ->
							CandidatesAccII;
						false ->
							remove_psb(CurrentPos, Num, CandidatesAccII)
					end
				end);
			{false, _OtherHouse} ->
				CandidatesAccI
		end
	end, Candidates, OccurencesMoreTwice).

-spec in_same_other_house(
	ThisHouse :: house(),
	Positions :: [board_position()]
) -> {InSame :: boolean(), OtherHouse :: house()}.

in_same_other_house(ThisHouse, Positions) ->
	case ThisHouse of
		Line when Line =:= row orelse Line =:= column ->
			{in_same_house(box, Positions), box};
		box ->
			case in_same_house(row, Positions) of
				true ->
					{true, row};
				false ->
					case in_same_house(column, Positions) of
						true ->
							{true, column};
						_NoLuck ->
							{false, undefined}
					end
			end
	end.

-spec in_same_house(
	Type :: house(),
	Positions :: [board_position()]
) -> boolean().

in_same_house(Type, [PosA, PosB]) ->
	XA = PosA rem 9,
	XB = PosB rem 9,
	YA = PosA div 9,
	YB = PosB div 9,
	case Type of
		row ->
			YA =:= YB;
		column ->
			XA =:= XB;
		box ->
			(XA div 3 =:= XB div 3) andalso (YA div 3 =:= YB div 3)
	end;
in_same_house(Type, [PosA, PosB | Rest]) ->
	in_same_house(Type, [PosA, PosB]) andalso in_same_house(Type, [PosB | Rest]).

%% ------------------------------------------------------------------------------------- %%
%% candidate_occurences_in_house
%% Helper function for remove_using_hidden_candidates and remove_using_box_line.
%% Collects all occurences of each number in the given house.

-spec candidate_occurences_in_house(
	Type :: house(),
	N :: house_index(),
	Candidates :: board_candidates()
) -> Occurences :: candidate_occurences().

candidate_occurences_in_house(Type, N, Candidates) ->
	for_nth_house(Type, N, dict:new(), fun(Pos, Occurences) ->
		CandidatesHere = candidates_to_list(Pos, Candidates),
		lists:foldl(fun(Num, OccurencesAcc) ->
			dict:append(Num, Pos, OccurencesAcc)
		end, Occurences, CandidatesHere)
	end).

%% ------------------------------------------------------------------------------------- %%
%% remove_using_pairs
%% Removes candidates by looking at Naked Pairs

-spec remove_using_pairs(
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

remove_using_pairs(Candidates) ->
	lists:foldl(fun(N, CandidatesAcc) ->
		Candidates0 = remove_using_pairs_in_house(row, N, CandidatesAcc),
		Candidates1 = remove_using_pairs_in_house(column, N, Candidates0),
		remove_using_pairs_in_house(box, N, Candidates1)
	end, Candidates, lists:seq(0, 8)).


-spec remove_using_pairs_in_house(
	Type :: house(),
	N :: house_index(),
	Candidates :: board_candidates()
) -> NewCandidates :: board_candidates().

remove_using_pairs_in_house(Type, N, Candidates) ->
	Occurences0 = pair_occurences_in_house(Type, N, Candidates),
	OccurencesTwice = dict:filter(fun(_Pair, Positions) ->
		length(Positions) =:= 2
	end, Occurences0),

	dict:fold(fun([NumA, NumB], Positions = [Pos1, Pos2], CandidatesAccI) ->
		Candidates1A = set_only_psb(Pos1, NumA, CandidatesAccI),
		Candidates1AB = add_psb(Pos1, NumB, Candidates1A),
		Candidates1AB2A = set_only_psb(Pos2, NumA, Candidates1AB),
		Candidates1AB2AB = add_psb(Pos2, NumB, Candidates1AB2A),
		lists:foldl(fun(Pos, CandidatesAccII) ->
			for_house_of(Type, Pos, CandidatesAccII, fun(CurrentPos, CandidatesAccIII) ->
				case CurrentPos =:= Pos1 orelse CurrentPos =:= Pos2 of
					true ->
						CandidatesAccIII;
					false ->
						CandidatesRemovedA = remove_psb(CurrentPos, NumA, CandidatesAccIII),
						remove_psb(CurrentPos, NumB, CandidatesRemovedA)
				end
			end)
		end, Candidates1AB2AB, Positions)
	end, Candidates, OccurencesTwice).

%% pair_occurences_in_house
%% Helper function forremove_using_pairs.
%% Collects all occurences of each naked candidatepair in the given house.

-spec pair_occurences_in_house(
	Type :: house(),
	N :: house_index(),
	Candidates :: board_candidates()
) -> Occurences :: pair_occurences().

pair_occurences_in_house(Type, N, Candidates) ->
	for_nth_house(Type, N, dict:new(), fun(Pos, Occurences) ->
		CandidatesHere = candidates_to_list(Pos, Candidates),
		case length(CandidatesHere) of
			2 ->
				dict:append(CandidatesHere, Pos, Occurences);
			_NotANakedPair ->
				Occurences
		end
	end).
	

%%% =================================================================================== %%%
%%% SUDOKU GENERATION                                                                   %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% generate_sudoku
%% Generates a sudoku by making holes in a filled out sudoku

-spec generate_sudoku() -> Board :: board().

generate_sudoku() ->
	Board0 = array:new([{size,81},{fixed,true}, {default, 0}]),

	random:seed(now()),

	BoardComplete = fill_board(Board0),

	remove_inferrables(BoardComplete, 3, 2).

%% ------------------------------------------------------------------------------------- %%
%% fill_board
%% Generates a filled out sudoku by backtracking.

-spec fill_board(
	BoardIn :: board()
) -> BoardOut :: board().

fill_board(Board) ->
	{true, Board1} = fill_board(0, Board, lists:seq(1, 9)),
	Board1.

-spec fill_board(
	Pos :: board_position()|81,
	BoardIn :: board(),
	Candidates :: [sudoku_number()]
) -> {Success :: boolean(), BoardOut :: board()}.

fill_board(81, Board, _Candidates) ->
	{true, Board};
fill_board(Pos, Board, Candidates) ->
	lists:foldl(fun(Num, Acc={SuccessYet, OldBoard}) ->
		case SuccessYet of
			true ->
				Acc;
			false ->
				{Valid, _Conflicts} = is_valid(Pos, Num, OldBoard),
				case Valid of
					true ->
						ExperimentalBoard = array:set(Pos, Num, OldBoard),
						{RecSuccess, NewBoard} = fill_board(Pos+1, ExperimentalBoard, shuffle(Candidates)),
						case RecSuccess of
							true ->
								{true, NewBoard};
							false ->
								{false, OldBoard}
						end;
					false ->
						{false, OldBoard}
				end
		end
	end, {false, Board}, Candidates).

% helper: shuffles a list

-spec shuffle(ListIn :: list(X)) -> ListOut :: list(X).

shuffle(List) ->
	Random_list = [{random:uniform(), X} || X <- List],
	[X || {_,X} <- lists:sort(Random_list)].

%% ------------------------------------------------------------------------------------- %%
%% remove_inferrables
%% Removes all given squares that can be inferred from other given squares
%% Always tries to make two holes at once, in a rotationally symmetric way.

-spec remove_inferrables(
	BoardIn :: board(),
	Sophistication :: 0..3,
	MaxHoles :: 0..81
) -> BoardOut :: board().

remove_inferrables(Board, Sophistication, MaxHoles) ->
	lists:foldl(fun(Pos, BoardAcc) ->
		BoardWithHoles = array:set(80-Pos, 0, array:set(Pos, 0 , BoardAcc)),
		Candidates = calculate_candidates(BoardWithHoles, Sophistication),

		% this fails if we remove too much during possibility calculation
		true = length(candidates_to_list(Pos, Candidates)) > 0,
		true = length(candidates_to_list(80-Pos, Candidates)) > 0,

		case length(candidates_to_list(Pos, Candidates)) =:= 1
		andalso length(candidates_to_list(80-Pos, Candidates)) =:= 1 of
			true ->
				BoardWithHoles;
			_MustBeGiven ->
				BoardAcc
		end
	end, Board, lists:seq(MaxHoles div 2, 0, -1)).


%%% =================================================================================== %%%
%%% GUESS CHECKING                                                                      %%%
%%% =================================================================================== %%%

-spec check_guess(
	Pos :: board_position(),
	Num :: sudoku_number(),
	Board :: board(),
	Candidates :: board_candidates()
) -> guess_result().

check_guess(Pos, Num, Board, Candidates) ->
	case array:get(Pos, Board) of
		0 ->
			case sdd_logic:is_valid(Pos, Num, Board) of
				{true, _NoConflicts} ->
					case sdd_logic:candidates_to_list(Pos, Candidates) of
						[Num] ->
							{good};
						_MoreOrOtherCandidates ->
							{ambigous, more_or_other_candidates}
					end;
				{false, Conflicts} ->
					{bad, Conflicts}
			end;
		_AlreadyFilled ->
			{already_filled}
	end.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(DIRTY_APPEND, fun(Pos, Acc) -> Acc ++ [Pos] end).

%% ------------------------------------------------------------------------------------- %%
%% for_house_of

for_house_of_test() ->
	?assertEqual(
	  [18,19,20,21,22,23,24,25,26],
	  for_house_of(row, 20, [], ?DIRTY_APPEND)
	),
	?assertEqual(
	  [2,11,20,29,38,47,56,65,74],
	  for_house_of(column, 20, [], ?DIRTY_APPEND)
	),
	?assertEqual(
	  [57,58,59,66,67,68,75,76,77],
	  for_house_of(box, 67, [], ?DIRTY_APPEND)
	).

%% ------------------------------------------------------------------------------------- %%
%% for_nth_house

for_nth_house_test() ->
	?assertEqual(
	  for_nth_house(row, 2, [], ?DIRTY_APPEND),
	  for_house_of(row, 20, [], ?DIRTY_APPEND)
	),
	?assertEqual(
	  for_nth_house(column, 2, [], ?DIRTY_APPEND),
	  for_house_of(column, 2, [], ?DIRTY_APPEND)
	),
	?assertEqual(
	  for_nth_house(box, 7, [], ?DIRTY_APPEND),
	  for_house_of(box, 67, [], ?DIRTY_APPEND)
	).

%% ------------------------------------------------------------------------------------- %%
%% for_houses_of

for_houses_of_test() ->
	?assertEqual(
	  [18, 19, 20, 21, 22, 23, 24, 25, 26,
	   2, 11, 20, 29, 38, 47, 56, 65, 74,
	   0, 1, 2, 9, 10, 11, 18, 19, 20],
	  for_houses_of(20, [], ?DIRTY_APPEND)
	).

%% ------------------------------------------------------------------------------------- %%
%% is_valid

is_valid_test() ->
	Board = array:from_list(
		[0,0,0,0,0,0,0,0,0,
		 0,0,0,0,5,0,3,6,4,
		 0,0,0,6,0,8,9,5,0,
		 0,4,0,0,0,1,0,0,9,
		 0,0,9,0,4,6,0,1,2,
		 0,1,0,7,0,9,5,0,3,
		 1,0,3,0,6,0,0,2,0,
		 2,5,6,3,1,4,8,9,7,
		 4,8,7,9,2,5,1,3,6]
	),
	?assertEqual({false, [54]}, is_valid(0, 1, Board)),
	?assertEqual({false, [56, 56]}, is_valid(55, 3, Board)),
	?assertEqual({true, []}, is_valid(55, 9, Board)).

%% ------------------------------------------------------------------------------------- %%
%% is_complete

is_complete_test() ->
	Board = array:from_list(
		[1,1,1,1,1,1,1,1,1,
		 1,1,1,1,1,1,1,1,1,
		 1,1,1,1,1,1,1,1,1,
		 1,1,1,1,1,1,1,1,1,
		 1,1,1,1,1,1,1,1,1,
		 1,1,1,1,1,1,1,1,1,
		 1,1,1,1,1,1,1,1,1,
		 1,1,1,1,1,1,1,1,1,
		 1,1,1,1,1,1,1,1,1]
	),
	?assertEqual(true, is_complete(Board)),
	Board1 = array:set(37, 0, Board),
	?assertEqual(false, is_complete(Board1)).

%% ------------------------------------------------------------------------------------- %%
%% has_psb

has_psb_test() ->
	?assertEqual(true, has_psb(0, 5, array:from_list([2#10010]))),
	?assertEqual(true, has_psb(0, 2, array:from_list([2#10010]))),
	?assertEqual(false, has_psb(0, 4, array:from_list([2#10010]))).

%% ------------------------------------------------------------------------------------- %%
%% add_psb

add_psb_test() ->
	NewCandidates = add_psb(0, 4, array:from_list([2#10000])),
	?assertEqual(false, has_psb(0, 2, NewCandidates)),
	?assertEqual(true, has_psb(0, 5, NewCandidates)),
	?assertEqual(true, has_psb(0, 4, NewCandidates)).

%% ------------------------------------------------------------------------------------- %%
%% remove_psb

remove_psb_test() ->
	NewCandidates = remove_psb(0, 4, array:from_list([2#11000])),
	?assertEqual(true, has_psb(0, 5, NewCandidates)),
	?assertEqual(false, has_psb(0, 4, NewCandidates)).

%% ------------------------------------------------------------------------------------- %%
%% set_only_psb

set_only_psb_test() ->
	NewCandidates = set_only_psb(0, 2, array:from_list([2#11000])),
	?assertEqual(false, has_psb(0, 5, NewCandidates)),
	?assertEqual(false, has_psb(0, 4, NewCandidates)),
	?assertEqual(true, has_psb(0, 2, NewCandidates)).

%% ------------------------------------------------------------------------------------- %%
%% calculate_candidates

calculate_candidates_test() ->
	BoardA = array:from_list(
		[0,0,0,0,0,0,0,0,0,
		 0,0,0,0,5,0,3,6,4,
		 0,0,0,6,0,8,9,5,0,
		 0,4,0,0,0,1,0,0,9,
		 0,0,9,0,4,6,0,1,2,
		 0,1,0,7,0,9,5,0,3,
		 1,0,3,0,6,0,0,2,0,
		 2,5,6,3,1,4,8,9,7,
		 4,8,7,9,2,5,1,3,6]
	),

	% Sophistication 0 (Naked Candidates)

	Candidates0 = calculate_candidates(BoardA, 0),
	?assertEqual(
		[4],
		candidates_to_list(68, Candidates0)
	),
	?assertEqual(
		[4],
		candidates_to_list(60, Candidates0)
	),
	?assertEqual(
		[3,5,6,7,8,9],
		candidates_to_list(0, Candidates0)
	),

	% Sophistication 1 (Hidden Candidates)

	Candidates1 = calculate_candidates(BoardA, 1),
	?assertEqual(
		[2],
		candidates_to_list(6, Candidates1)
	),
	?assertEqual(
		[4],
		candidates_to_list(52, Candidates1)
	),
	?assertEqual(
		[2],
		candidates_to_list(47, Candidates1)
	),

	% Sophistication 2
	
	% sudoku used: http://www.sudokuwiki.org/sudoku.htm?bd=016007803090800000870001260048000300650009082039000650060900020080002936924600510
	
	BoardB = array:from_list(
			[0,1,6,0,0,7,8,0,3,
			 0,9,0,8,0,0,0,0,0,
			 8,7,0,0,0,1,2,6,0,
			 0,4,8,0,0,0,3,0,0,
			 6,5,0,0,0,9,0,8,2,
			 0,3,9,0,0,0,6,5,0,
			 0,6,0,9,0,0,0,2,0,
			 0,8,0,0,0,2,9,3,6,
			 9,2,4,6,0,0,5,1,0]
	),

	Candidates2 = calculate_candidates(BoardB, 2),

	% Box-Line (Pointing Pairs)
	% (Take 4 Steps in solver above)
	?assertEqual(
		true,
		has_psb(54, 3, Candidates2)
	),
	?assertEqual(
		true,
		has_psb(56, 3, Candidates2)
	),
	?assertEqual(
		false,
		has_psb(58, 3, Candidates2)
	),
	?assertEqual(
		false,
		has_psb(59, 3, Candidates2)
	),

	% Line-Box
	% (Take 14 Steps in solver above)
	?assertEqual(
		true,
		has_psb(7, 4, Candidates2)
	),
	?assertEqual(
		true,
		has_psb(16, 4, Candidates2)
	),
	?assertEqual(
		false,
		has_psb(15, 4, Candidates2)
	),
	?assertEqual(
		false,
		has_psb(17, 4, Candidates2)
	),
	?assertEqual(
		false,
		has_psb(26, 4, Candidates2)
	),

	% Sophie's game
	% https://www.facebook.com/groups/237967166319281/permalink/279823868800277/
	% tests that hidden candidate removal is applied again after Box-Line
	BoardSofie = array:from_list(
			[0,0,8,0,0,0,6,2,4,
			 1,2,0,0,3,4,5,8,7,
			 0,0,7,0,0,0,9,1,3,
			 7,0,3,4,2,0,8,9,5,
			 0,5,0,0,0,7,3,6,2,
			 0,0,2,3,0,0,4,7,1,
			 0,0,0,8,1,2,7,0,9,
			 9,8,1,0,0,0,2,0,6,
			 2,7,0,0,0,0,1,5,8]
	),

	CandidatesSofie = calculate_candidates(BoardSofie, 2),
	?assertEqual(
		true,
		has_psb(54, 3, CandidatesSofie)
	),
	?assertEqual(
		true,
		has_psb(55, 3, CandidatesSofie)
	),
	?assertEqual(
		[4],
		candidates_to_list(61, CandidatesSofie)
	),
	?assertEqual(
		[3],
		candidates_to_list(70, CandidatesSofie)
	),

	% Sophistication 3 (Naked Pairs)
	% sudoku used: http://www.sudokuwiki.org/sudoku.htm?bd=400000938032094100095300240370609004529001673604703090957008300003900400240030709
	% (from the description of naked pairs method)

	BoardC = array:from_list(
			[4,0,0,0,0,0,9,3,8,
			 0,3,2,0,9,4,1,0,0,
			 0,9,5,3,0,0,2,4,0,
			 3,7,0,6,0,9,0,0,4,
			 5,2,9,0,0,1,6,7,3,
			 6,0,4,7,0,3,0,9,0,
			 9,5,7,0,0,8,3,0,0,
			 0,0,3,9,0,0,4,0,0,
			 2,4,0,0,3,0,7,0,9]
	),

	Candidates3 = calculate_candidates(BoardC, 3),
	?assertEqual(
		false,
		has_psb(3, 1, Candidates3)
	),
	?assertEqual(
		false,
		has_psb(4, 1, Candidates3)
	),
	?assertEqual(
		false,
		has_psb(4, 6, Candidates3)
	),
	?assertEqual(
		false,
		has_psb(5, 6, Candidates3)
	),
	?assertEqual(
		false,
		has_psb(18, 1, Candidates3)
	).

-endif.