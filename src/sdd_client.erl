%%% =================================================================================== %%%
%%% Sudoku Dash Game: Client Server                                                     %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is a gen_server that handles one client session                                %%%
%%% - collects messages from player and game                                            %%%
%%% - encodes and sends them to connected bullet servers                                %%%
%%% =================================================================================== %%%

-module(sdd_client).

%% Records
-record(state, {
	player,
	current_game,
	current_connection,
	current_connection_active,
	messages_for_client = []
}).

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("sdd_history_test_macros.hrl").

init_createsNewClientForPlayerAndConnectsToHim_test() ->
	meck:new(sdd_player),
	meck:expect(sdd_player, connect, fun
		(_PlayerId, _ClientId, _ClientInfo) -> ok
	end),

	{ok, InitialState} = init({"ConnectionA", true, "ClientId", "ClientInfo", "Peter"}),

	?assert(meck:called(sdd_player, connect, ["Peter", "ClientId", "ClientInfo"])),
	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player),

	?assertEqual(InitialState#state.current_connection, "ConnectionA"),
	?assertEqual(InitialState#state.current_connection_active, true),
	?assertEqual(InitialState#state.player, "Peter").

-endif.