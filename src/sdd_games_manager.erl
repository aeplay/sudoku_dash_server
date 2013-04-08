%%% =================================================================================== %%%
%%% Sudoku Dash: Games Manager                                                          %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is a gen_server that manages running games and distributes players amongs them %%%
%%% =================================================================================== %%%

-module(sdd_games_manager).

%% API
-export([start_link/0, find_game_and_join/4, leave/3, remove_game/1]).

%% GEN_SERVER
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

%% Types and Records
-record(state, {
	games = []
}).

-ifdef(TEST).
-define(MAX_PLAYERS_PER_GAME, 2).
-else.
-define(MAX_PLAYERS_PER_GAME, 3).
-endif.

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

find_game_and_join(PlayerId, PlayerInfo, _Options, CurrentGame) ->
	gen_server:cast(?MODULE, {find_game_and_join, PlayerId, PlayerInfo, CurrentGame}).

leave(PlayerId, GameId, Reason) ->
	gen_server:cast(?MODULE, {leave, PlayerId, GameId, Reason}).

remove_game(GameId) ->
	gen_server:cast(?MODULE, {remove_game, GameId}).

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

init(_Opts) ->
	{ok, #state{}}.

%% ------------------------------------------------------------------------------------- %%
%% Tries to rejoin a player in his current game, or to find a game, or to create a game


handle_cast({find_game_and_join, PlayerId, PlayerInfo, CurrentGame}, State) ->
	StateAfterLeave = leave(PlayerId, CurrentGame, rejoin, State),
	StateAfterJoin = case join(PlayerId, PlayerInfo, CurrentGame, rejoin, StateAfterLeave) of
		{true, StateAfterReJoin} -> StateAfterReJoin;
		{false, _} ->
			OpenGames = lists:filter(fun({_GameId, NPlayers}) -> NPlayers < ?MAX_PLAYERS_PER_GAME end, StateAfterLeave#state.games),
			{GameToJoin, NewState} = case OpenGames of
				[{Game, _NPlayers}] -> {Game, StateAfterLeave};
				[{Game, _NPlayers} | _] -> {Game, StateAfterLeave};
				[] -> create_game(StateAfterLeave)
			end,
			{_Result, StateAfterNewJoin} = join(PlayerId, PlayerInfo, GameToJoin, random, NewState),
			StateAfterNewJoin
	end,
	{noreply, StateAfterJoin};

%% ------------------------------------------------------------------------------------- %%
%% Removes a player from a game and decreases the according player counter

handle_cast({leave, PlayerId, GameId, Reason}, State) ->
	{noreply, leave(PlayerId, GameId, Reason, State)};

%% ------------------------------------------------------------------------------------- %%
%% Removes a game from the list

handle_cast({remove_game, GameId},  State) ->
	NewGames = lists:keydelete(GameId, 1, State#state.games),
	{noreply, State#state{games = NewGames}}.

%% ------------------------------------------------------------------------------------- %%
%% Rest of gen_server calls
handle_call(_Reg, _From, State) ->
	State.

handle_info(_Info, State) ->
	State.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) -> ok.

%%% =================================================================================== %%%
%%% UTILITY FUNCTIONS                                                                   %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Starts a new game and registers it

create_game(State) ->
	GameId = uuid:to_string(uuid:uuid4()),
	sdd_games_sup:start_game(GameId),
	Games = State#state.games,
	NewGames = [{GameId, 0}|Games],
	{GameId, State#state{games = NewGames}}.

join(PlayerId, PlayerInfo, GameId, Source, State) ->
	case lists:keyfind(GameId, 1, State#state.games) of
		false -> {false, State};
		{GameId, OldPlayerCount} ->
			case OldPlayerCount < ?MAX_PLAYERS_PER_GAME of
				false ->
					{false, State};
				true ->
					sdd_player:do(PlayerId, join, {GameId, Source}),
					case sdd_game:join(GameId, PlayerId, PlayerInfo, Source) of
						ok ->
							NewGames = lists:keyreplace(GameId, 1, State#state.games, {GameId, OldPlayerCount + 1}),
							{true, State#state{games = NewGames}};
						_Error ->
							sdd_player:do(PlayerId, leave, join_error),
							{false, State}
					end
			end
	end.

leave(PlayerId, GameId, Reason, State) ->
	sdd_player:do(PlayerId, leave, Reason),
	sdd_game:do(GameId, PlayerId, leave, Reason),
	case lists:keyfind(GameId, 1, State#state.games) of
		false -> State;
		{GameId, OldPlayerCount} ->
			NewGames = lists:keyreplace(GameId, 1, State#state.games, {GameId, OldPlayerCount - 1}),
			State#state{games = NewGames}
	end.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(meck_sdd_game,
	meck:new(sdd_game),
	meck:sequence(sdd_game, start_link, 1, ["GameA", "GameB", "GameC"]),
	meck:expect(sdd_game, join, fun
		(_GameId, "Paul", _PlayerInfo, _Source) -> nope;
		(_GameId, _PlayerId, _PlayerInfo, _Source) -> ok
	end),
	meck:expect(sdd_game, do, fun
		(_, _, _, _) -> ok
	end)
).

-define(meck_sdd_games_sup,
	meck:new(sdd_games_sup),
	meck:expect(sdd_games_sup, start_game, fun(_GameId) -> ok end)
).

init_initializes_test() ->
	?assertEqual({ok, #state{}}, init([])).

find_game_and_join_triesToRejoinAPlayerToHisCurrentGame_test() ->
	?meck_sdd_game,

	InitialState = #state{games=[{"OldGame", 0}]},

	handle_cast({find_game_and_join, "Peter", "PeterInfo", "OldGame"}, InitialState),
	?assert(meck:called(sdd_game, do, ["OldGame", "Peter", leave, rejoin])),
	?assert(meck:called(sdd_game, join, ["OldGame", "Peter", "PeterInfo", rejoin])),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

find_game_and_join_triesToJoinAPlayerToAnOpenGame_test() ->
	?meck_sdd_game,

	InitialState = #state{games=[{"FullGame", 5}, {"OpenGame", 0}]},

	handle_cast({find_game_and_join, "Peter", "PeterInfo", "OldGame"}, InitialState),
	?assert(meck:called(sdd_game, join, ["OpenGame", "Peter", "PeterInfo", random])),

	InitialState2 = #state{games=[{"FullGame", 5}, {"OpenGame1", 0}, {"OpenGame2", 0}]},

	handle_cast({find_game_and_join, "Peter", "PeterInfo", "OldGame"}, InitialState2),
	?assert(meck:called(sdd_game, join, ["OpenGame1", "Peter", "PeterInfo", random])),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

find_game_and_join_createsANewGameIfThereAreNoOpenGames_test() ->
	?meck_sdd_game,
	?meck_sdd_games_sup,

	InitialState = #state{games=[{"FullGame1", 5}, {"FullGame2", 4}]},

	{noreply, StateAfterJoin} = handle_cast({find_game_and_join, "Peter", "PeterInfo", "OldGame"}, InitialState),
	#state{games=[{NewGame, 1} | _OldGames]} = StateAfterJoin,
	?assert(meck:called(sdd_game, join, [NewGame, "Peter", "PeterInfo", random])),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),
	meck:unload(sdd_games_sup).

remove_game_removesAGames_test() ->
	InitialState = #state{games=[{"GameA", 0}, {"GameB", 0}]},

	{noreply, StateAfterRemove} = handle_cast({remove_game, "GameA"}, InitialState),
	?assertEqual(#state{games=[{"GameB", 0}]}, StateAfterRemove).

create_game_startsGameAndRegistersIt_test() ->
	?meck_sdd_games_sup,

	InitialState = #state{},
	{GameId, NewState} = create_game(InitialState),

	?assert(meck:called(sdd_games_sup, start_game, '_')),
	?assert(meck:validate(sdd_games_sup)),
	meck:unload(sdd_games_sup),

	?assertEqual({GameId, 0}, lists:keyfind(GameId, 1, NewState#state.games)).

join_joinsPlayerToGameAndIncreasesPlayerCountIfSuccessful_test() ->
	?meck_sdd_game,
	?meck_sdd_games_sup,
	{GameId, InitialState} = create_game(#state{}),

	{BadResult, StateAfterBadJoin} = join("Paul", "PaulInfo", GameId, random, InitialState),

	?assert(meck:called(sdd_game, join, [GameId, "Paul", "PaulInfo", random])),

	?assertEqual(false, BadResult),
	?assertEqual({GameId, 0}, lists:keyfind(GameId, 1, StateAfterBadJoin#state.games)),

	{GoodResult, StateAfterGoodJoin} = join("Peter", "PeterInfo", GameId, random, InitialState),

	?assert(meck:called(sdd_game, join, [GameId, "Peter", "PeterInfo", random])),

	?assertEqual(true, GoodResult),
	?assertEqual({GameId, 1}, lists:keyfind(GameId, 1, StateAfterGoodJoin#state.games)),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),
	meck:unload(sdd_games_sup).

join_failsIfGameIsFull_test() ->
	?meck_sdd_game,
	?meck_sdd_games_sup,
	{GameId, InitialState} = create_game(#state{}),

	{true, StateAfterFirstJoin} = join("Peter", "PeterInfo", GameId, random, InitialState),
	{true, StateAfterSecondJoin} = join("Petra", "PetraInfo", GameId, random, StateAfterFirstJoin),
	{ResultWhenFull, StateAfterThirdJoin} = join("Petrus", "PetrusInfo", GameId, random, StateAfterSecondJoin),

	?assertNot(meck:called(sdd_game, join, [GameId, "Petrus", "PetrusInfo", random])),

	?assertEqual(false, ResultWhenFull),
	?assertEqual({GameId, 2}, lists:keyfind(GameId, 1, StateAfterThirdJoin#state.games)),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),
	meck:unload(sdd_games_sup).

leave_removesPlayerFromGameAndDecreasesPlayerCount_test() ->
	?meck_sdd_game,
	?meck_sdd_games_sup,
	{GameId, InitialState} = create_game(#state{}),
	{true, StateAfterJoin} = join("Peter", "PeterInfo", GameId, random, InitialState),

	{noreply, StateAfterLeave} = handle_cast({leave, "Peter", GameId, fell_asleep}, StateAfterJoin),

	?assert(meck:called(sdd_game, do, [GameId, "Peter", leave, fell_asleep])),
	?assertEqual({GameId, 0}, lists:keyfind(GameId, 1, StateAfterLeave#state.games)),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),
	meck:unload(sdd_games_sup).

-endif.