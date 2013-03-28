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
	id,
	connection,
	connection_active,
	connection_can_send,
	messages_for_client = [],
	player,
	current_game
}).

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Creates a new client with client info and connects to the given player

init({ClientId, ClientInfo, PlayerId}) ->
	InitialState = #state{
		id = ClientId,
		player = PlayerId
	},
	sdd_player:connect(PlayerId, ClientId, ClientInfo),
	{ok, InitialState}.

%% ------------------------------------------------------------------------------------- %%
%% Adds a new connection, remembers its active state and resets whether we can send

handle_cast({add_connection, ConnectionPid, ConnectionActive}, State) ->
	NewState = State#state{
		connection = ConnectionPid,
		connection_active = ConnectionActive,
		connection_can_send = true
	},
	ConnectionPid ! send_hello,
	{noreply, NewState}.

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

	{ok, InitialState} = init({"ClientId", "ClientInfo", "Peter"}),

	?assert(meck:called(sdd_player, connect, ["Peter", "ClientId", "ClientInfo"])),
	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player),

	?assertEqual(InitialState#state.id, "ClientId"),
	?assertEqual(InitialState#state.player, "Peter").

add_connection_setsNewConnectionSavesItsActiveStateAndRepliesWithHello_test() ->
	{noreply, State} = handle_cast({add_connection, self(), true}, #state{}),

	?assertEqual(self(), State#state.connection),
	?assertEqual(true, State#state.connection_active),
	?assertEqual(true, State#state.connection_can_send),

	receive
		send_hello -> ?assert(true)
	after
		100 -> ?assert(false)
	end.


-endif.