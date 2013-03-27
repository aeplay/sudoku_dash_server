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

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

create_game_startsGameAndRegistersIt_test() ->
	meck:new(sdd_game),
	meck:sequence(sdd_game, start, 1, ["GameA", "GameB", "GameC"]),

	InitialState = #state{},
	{GameId, NewState} = create_game(InitialState),

	?assert(meck:called(sdd_game, start, [no_options])),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),

	?assertEqual("GameA", GameId),
	?assertEqual(0, gb_trees:get("GameA", NewState#state.games)).

-endif.