%%% =================================================================================== %%%
%%% Sudoku Dash Game: Games Supervisor                                                  %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is the supervisor for all game processes                                       %%%
%%% =================================================================================== %%%

-module(sdd_games_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,  start_game/1]).

%% Supervisor callbacks
-export([init/1]).

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_game(Id) ->
	supervisor:start_child(?MODULE, [Id]).

%%% =================================================================================== %%%
%%% SUPERVISOR CALLBACKS                                                                %%%
%%% =================================================================================== %%%

init([]) ->
	GameSpec = {
		game,
		{sdd_game, start_link, []},
		temporary,
		1000,
		worker,
		[sdd_game]
	},
	{ok, {{simple_one_for_one, 5, 10}, [GameSpec]}}.