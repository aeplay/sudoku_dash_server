%%% =================================================================================== %%%
%%% Sudoku Dash: Games Manager                                                          %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is a gen_server that manages running games and distributes players amongs them %%%
%%% =================================================================================== %%%

-module(sdd_games_manager).

%% API
-export([start_link/0, find_game_and_join/2, rejoin/2]).

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
-define(MAX_PLAYERS_PER_GAME, 5).
-endif.

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

find_game_and_join(PlayerId, _Options) ->
	gen_server:cast(?MODULE, {find_game_and_join, PlayerId}).

rejoin(PlayerId, GameId) ->
	gen_server:cast(?MODULE, {rejoin, PlayerId, GameId}).

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

init(_Opts) ->
	{ok, #state{}}.

handle_cast({find_game_and_join, PlayerId}, State) ->
	OpenGames = lists:filter(fun({_GameId, NPlayers}) -> NPlayers < ?MAX_PLAYERS_PER_GAME end, State#state.games),
	{GameToJoin, NewState} = case OpenGames of
		[{Game, _NPlayers}] -> {Game, State};
		[{Game, _NPlayers} | _] -> {Game, State};
		[] -> create_game(State)
	end,
	StateAfterJoin = join(PlayerId, GameToJoin, random, NewState),
	{noreply, StateAfterJoin};

%% ------------------------------------------------------------------------------------- %%
%% Removes a player from a game and decreases the according player counter

handle_cast({leave, PlayerId, GameId, Reason}, State) ->
	{noreply, leave(PlayerId, GameId, Reason, State)};

%% ------------------------------------------------------------------------------------- %%
%% Removes a player from a game and decreases the according player counter

handle_cast({rejoin, PlayerId, GameId}, State) ->
	StateAfterLeave = leave(PlayerId, GameId, rejoin, State),
	StateAfterJoin = join(PlayerId, GameId, rejoin, StateAfterLeave),
	{noreply, StateAfterJoin}.

%% ------------------------------------------------------------------------------------- %%
%% Rest of gen_server calls

handle_call(_Req, _From, State) ->
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

join(PlayerId, GameId, Source, State) ->
	{GameId, OldPlayerCount} = lists:keyfind(GameId, 1, State#state.games),
	case OldPlayerCount < ?MAX_PLAYERS_PER_GAME of
		true ->
			sdd_player:do(PlayerId, join, {GameId, Source}),
			case sdd_game:join(GameId, PlayerId, Source) of
				ok ->
					NewGames = lists:keyreplace(GameId, 1, State#state.games, {GameId, OldPlayerCount + 1}),
					State#state{games = NewGames};
				_Error ->
					sdd_player:do(PlayerId, leave, join_error),
					State
			end;
		false ->
			State
	end.

leave(PlayerId, GameId, Reason, State) ->
	sdd_player:do(PlayerId, leave, Reason),
	sdd_game:do(GameId, PlayerId, leave, Reason),
	{GameId, OldPlayerCount} = lists:keyfind(GameId, 1, State#state.games),
	NewGames = lists:keyreplace(GameId, 1, State#state.games, {GameId, OldPlayerCount - 1}),
	State#state{games = NewGames}.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(meck_sdd_game,
	meck:new(sdd_game),
	meck:sequence(sdd_game, start_link, 1, ["GameA", "GameB", "GameC"]),
	meck:expect(sdd_game, do, fun
		(_GameId, "Paul", _Action, _Args) -> nope;
		(_GameId, _PlayerId, _Action, _Args) -> ok
	end)
).

create_game_startsGameAndRegistersIt_test() ->
	?meck_sdd_game,

	InitialState = #state{},
	{GameId, NewState} = create_game(InitialState),

	?assert(meck:called(sdd_game, start_link, [no_options])),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),

	?assertEqual("GameA", GameId),
	?assertEqual({"GameA", 0}, lists:keyfind("GameA", 1, NewState#state.games)).

join_joinsPlayerToGameAndIncreasesPlayerCountIfSuccessful_test() ->
	?meck_sdd_game,
	{GameId, InitialState} = create_game(#state{}),

	{BadResult, StateAfterBadJoin} = join("Paul", GameId, random, InitialState),

	?assert(meck:called(sdd_game, do, [GameId, "Paul", join, random])),

	?assertEqual(nope, BadResult),
	?assertEqual({"GameA", 0}, lists:keyfind("GameA", 1, StateAfterBadJoin#state.games)),

	{GoodResult, StateAfterGoodJoin} = join("Peter", GameId, random, InitialState),

	?assert(meck:called(sdd_game, do, [GameId, "Peter", join, random])),

	?assertEqual(ok, GoodResult),
	?assertEqual({"GameA", 1}, lists:keyfind("GameA", 1, StateAfterGoodJoin#state.games)),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

join_failsIfGameIsFull_test() ->
	?meck_sdd_game,
	{GameId, InitialState} = create_game(#state{}),

	{ok, StateAfterFirstJoin} = join("Peter", GameId, random, InitialState),
	{ok, StateAfterSecondJoin} = join("Petra", GameId, random, StateAfterFirstJoin),
	{ResultWhenFull, StateAfterThirdJoin} = join("Petrus", GameId, random, StateAfterSecondJoin),

	?assertNot(meck:called(sdd_game, do, [GameId, "Petrus", join, random])),

	?assertEqual(game_full, ResultWhenFull),
	?assertEqual({"GameA", 2}, lists:keyfind("GameA", 1, StateAfterThirdJoin#state.games)),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

leave_removesPlayerFromGameAndDecreasesPlayerCount_test() ->
	?meck_sdd_game,
	{GameId, InitialState} = create_game(#state{}),
	{ok, StateAfterJoin} = join("Peter", GameId, random, InitialState),

	{noreply, StateAfterLeave} = handle_cast({leave, "Peter", GameId, fell_asleep}, StateAfterJoin),

	?assert(meck:called(sdd_game, do, [GameId, "Peter", leave, fell_asleep])),
	?assertEqual({"GameA", 0}, lists:keyfind("GameA", 1, StateAfterLeave#state.games)),

	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game).

-endif.