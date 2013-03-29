%%% =================================================================================== %%%
%%% Sudoku Dash Game: Clients Supervisor                                                 %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is the supervisor for all client processes                                     %%%
%%% =================================================================================== %%%

-module(sdd_clients_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,  start_client/1]).

%% Supervisor callbacks
-export([init/1]).

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(ClientId) ->
	supervisor:start_child(?MODULE, [ClientId, noinfo]).

%%% =================================================================================== %%%
%%% SUPERVISOR CALLBACKS                                                                %%%
%%% =================================================================================== %%%

init([]) ->
	ClientSpec = {
		client,
		{sdd_client, start_link, []},
		temporary,
		1000,
		worker,
		[sdd_player]
	},
	{ok, {{simple_one_for_one, 5, 10}, [ClientSpec]}}.