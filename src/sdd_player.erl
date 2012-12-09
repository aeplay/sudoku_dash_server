%%% =================================================================================== %%%
%%% Sudoku Dash Game: Player Server                                                     %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is a gen_server that handles a connected player:                               %%%
%%% - holds temporary player state                                                      %%%
%%% - connects game and client                                                          %%%
%%% It only lives as long as the client processes it is connected to although it can    %%%
%%% handle temporary disconnects of the client.                                         %%%
%%% =================================================================================== %%%

-module(sdd_player).

%% API
-export([handle_game_event/3]).

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

handle_game_event(PlayerId, EventType, EventData) ->
	try gen_server:call(PlayerId, {game_event, EventType, EventData}, 100) of
		Reply -> Reply
	catch
		Error -> Error
	end.