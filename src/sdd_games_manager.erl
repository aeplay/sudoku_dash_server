%%% =================================================================================== %%%
%%% Sudoku Dash: Games Manager                                                          %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is a gen_server that manages running games and distributes players amongs them %%%
%%% =================================================================================== %%%

-module(sdd_games_manager).

%% Types and Records
-record(state, {
	games = gb_trees:empty()
}).

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

%%% =================================================================================== %%%
%%% UTILITY FUNCTIONS                                                                   %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Starts a new game and registers it

create_game(State) ->
	GameId = sdd_game:start(no_options),
	Games = State#state.games,
	NewGames = gb_trees:insert(GameId, 0, Games),
	{GameId, State#state{games = NewGames}}.

join(PlayerId, GameId, Source, State) ->
	case sdd_game:do(GameId, PlayerId, join, Source) of
		ok ->
			OldPlayerCount = gb_trees:get(GameId, State#state.games),
			NewGames = gb_trees:update(GameId, OldPlayerCount + 1, State#state.games),
			{ok, State#state{games = NewGames}};
		_Error ->
			{_Error, State}
	end.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(meck_sdd_game,
	meck:new(sdd_game),
	meck:sequence(sdd_game, start, 1, ["GameA", "GameB", "GameC"]),
	meck:expect(sdd_game, do, fun
		(_GameId, "Paul", _Action, _Args) -> nope;
		(_GameId, _PlayerId, _Action, _Args) -> ok
	end)
).

create_game_startsGameAndRegistersIt_test() ->
	?meck_sdd_game,

	InitialState = #state{},
	{GameId, NewState} = create_game(InitialState),

	?assert(meck:called(sdd_game, start, [no_options])),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),

	?assertEqual("GameA", GameId),
	?assertEqual(0, gb_trees:get("GameA", NewState#state.games)).

join_joinsPlayerToGameAndIncreasesPlayerCountIfSuccessful_test() ->
	?meck_sdd_game,
	{GameId, InitialState} = create_game(#state{}),

	{BadResult, StateAfterBadJoin} = join("Paul", GameId, random, InitialState),

	?assert(meck:called(sdd_game, do, [GameId, "Paul", join, random])),

	?assertEqual(nope, BadResult),
	?assertEqual(0, gb_trees:get("GameA", StateAfterBadJoin#state.games)),

	{GoodResult, StateAfterGoodJoin} = join("Peter", GameId, random, InitialState),

	?assert(meck:called(sdd_game, do, [GameId, "Peter", join, random])),

	?assertEqual(ok, GoodResult),
	?assertEqual(1, gb_trees:get("GameA", StateAfterGoodJoin#state.games)),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

join_failsIfGameIsFull_test() ->
	?meck_sdd_game,
	{GameId, InitialState} = create_game(#state{}),

	{ok, StateAfterFirstJoin} = join("Peter", GameId, random, InitialState),
	{ok, StateAfterSecondJoin} = join("Petra", GameId, random, StateAfterFirstJoin),
	{ResultWhenFull, StateAfterThirdJoin} = join("Petrus", GameId, random, StateAfterSecondJoin),

	?assertEqual(game_full, ResultWhenFull),
	?assertEqual(2, gb_trees:get("GameA", StateAfterThirdJoin#state.games)),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

-endif.