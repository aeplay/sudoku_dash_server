%%% =================================================================================== %%%
%%% Sudoku Dash Game: Players Supervisor                                                %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is the supervisor for all player processes                                     %%%
%%% =================================================================================== %%%

-module(sdd_players_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,  start_player/2]).

%% Supervisor callbacks
-export([init/1]).

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_player(PlayerId, History) ->
	supervisor:start_child(?MODULE, [PlayerId, History]).

%%% =================================================================================== %%%
%%% SUPERVISOR CALLBACKS                                                                %%%
%%% =================================================================================== %%%

init([]) ->
	PlayerSpec = {
		player,
		{sdd_player, start_link, []},
		temporary,
		1000,
		worker,
		[sdd_player]
	},
	{ok, {{simple_one_for_one, 5, 10}, [PlayerSpec]}}.