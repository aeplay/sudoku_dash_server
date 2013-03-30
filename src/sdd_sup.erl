%%% =================================================================================== %%%
%%% Sudoku Dash Game: Main Supervisor                                                   %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is the supervisor for all other supervisors                                    %%%
%%% =================================================================================== %%%

-module(sdd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% =================================================================================== %%%
%%% SUPERVISOR CALLBACKS                                                                %%%
%%% =================================================================================== %%%

init([]) ->
	ClientSupSpec = {
		clients_sup,
		{sdd_clients_sup, start_link, []},
		permanent,
		1000,
		supervisor,
		[sdd_clients_sup]
	},
	PlayerSupSpec = {
		players_sup,
		{sdd_players_sup, start_link, []},
		permanent,
		1000,
		supervisor,
		[sdd_players_sup]
	},
	GamesManagerSpec = {
		games_manager,
		{sdd_games_manager, start_link, []},
		permanent,
		1000,
		worker,
		[sdd_games_manager]
	},
	GamesSupSpec = {
		games_sup,
		{sdd_games_sup, start_link, []},
		permanent,
		1000,
		supervisor,
		[sdd_games_sup]
	},
	{ok, {{one_for_one, 5, 10}, [ClientSupSpec, PlayerSupSpec, GamesManagerSpec, GamesSupSpec]}}.