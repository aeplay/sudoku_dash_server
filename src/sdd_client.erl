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
	info,
	connection,
	connection_active,
	connection_can_send,
	messages = [],
	player,
	current_game
}).

%%% =================================================================================== %%%
%%% GEN_SERVER CALLBACKS                                                                %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Creates a new client with client info and connects to the given player

init({ClientId, ClientInfo}) ->
	InitialState = #state{
		id = ClientId,
		info = ClientInfo
	},
	{ok, InitialState}.

%% ------------------------------------------------------------------------------------- %%
%% Tries to authenticate with a secret and connect to the given player

handle_call({login, PlayerId, Secret}, _From, State) ->
	case sdd_player:authenticate(PlayerId, Secret) of
		true -> 
			sdd_player:connect(PlayerId, State#state.id, State#state.info),
			{reply, ok, State#state{player = PlayerId}};
		false -> {reply, authentication_failed, State}
	end.

%% ------------------------------------------------------------------------------------- %%
%% Adds a new connection, remembers its active state and resets whether we can send

handle_cast({add_connection, ConnectionPid, ConnectionActive}, State) ->
	NewState = State#state{
		connection = ConnectionPid,
		connection_active = ConnectionActive,
		connection_can_send = true
	},
	StateAfterHelloSent = add_message(hello, NewState),
	{noreply, StateAfterHelloSent};

%% Makes the player do something on behalf of the client

handle_cast({player_do, Action, Args}, State) ->
	sdd_player:do(State#state.player, Action, Args),
	{noreply, State}.

%%% =================================================================================== %%%
%%% UTILITY FUNCTION                                                                    %%%
%%% =================================================================================== %%%

add_message(Message, State) ->
	case {State#state.connection, State#state.connection_can_send} of
		{undefined, _} -> State;
		{_Connection, false} -> State;
		{Connection, true} ->
			Messages = [Message | State#state.messages],
			Connection ! {messages, Messages},
			case State#state.connection_active of
				true -> State#state{messages = []};
				false -> State#state{messages = [], connection_can_send = false}
			end
	end.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("sdd_history_test_macros.hrl").

-define(meck_sdd_player,
	meck:new(sdd_player),
	meck:expect(sdd_player, authenticate, fun
		("Peter", "GoodSecret") -> true;
		("Peter", "BadSecret") -> false
	end),
	meck:expect(sdd_player, connect, fun
		(_PlayerId, _ClientId, _ClientInfo) -> ok
	end),
	meck:expect(sdd_player, do, fun
		(_PlayerId, _Action, _Args) -> ok
	end)
).

init_savesClientIdAndInfo_test() ->
	{ok, InitialState} = init({"ClientId", "ClientInfo"}),
	?assertEqual("ClientId", InitialState#state.id),
	?assertEqual("ClientInfo", InitialState#state.info).

login_authenticatesWithPlayerAndConnectsIfSuccessful_test() ->	
	?meck_sdd_player,

	InitialState = #state{id = "ClientId", info = "ClientInfo", player = undefined},

	{reply, authentication_failed, StateAfterBadLogin} = handle_call({login, "Peter", "BadSecret"}, from, InitialState),

	?assert(meck:called(sdd_player, authenticate, ["Peter", "BadSecret"])),
	?assertNot(meck:called(sdd_player, connect, ["Peter", "ClientId", "ClientInfo"])),
	?assertEqual(StateAfterBadLogin#state.player, undefined),

	{reply, ok, StateAfterGoodLogin} = handle_call({login, "Peter", "GoodSecret"}, from, InitialState),

	?assert(meck:called(sdd_player, authenticate, ["Peter", "GoodSecret"])),
	?assert(meck:called(sdd_player, connect, ["Peter", "ClientId", "ClientInfo"])),
	?assertEqual(StateAfterGoodLogin#state.player, "Peter"),

	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player).

add_connection_setsNewConnectionSavesItsActiveStateAndRepliesWithHello_test() ->
	{noreply, State} = handle_cast({add_connection, self(), true}, #state{}),

	?assertEqual(self(), State#state.connection),
	?assertEqual(true, State#state.connection_active),
	?assertEqual(true, State#state.connection_can_send),

	receive
		{messages, [hello]} -> ?assert(true)
	after
		100 -> ?assert(false)
	end.

player_do_forwardsActionToPlayer_test() ->
	?meck_sdd_player,
	State = #state{player = "Peter"},
	{noreply, StateAfterDo} = handle_cast({player_do, "Action", "Args"}, State),

	?assert(meck:called(sdd_player, do, ["Peter", "Action", "Args"])),
	?assertEqual(State, StateAfterDo),

	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player).

add_message_failsIfNoConnection_test() ->
	?assertEqual(#state{}, add_message(message, #state{})).

add_message_failsIfCantSendAnymore_test() ->
	State = #state{connection = "SomeConnection", connection_can_send = false},
	?assertEqual(State, add_message(message, State)).

add_message_canSendOneBatchOfMessagesIfCanSendButNotActive_test() ->
	State = #state{
		connection = self(),
		connection_active = false,
		connection_can_send = true,
		messages = [message_a]
	},
	StateAfterAddMessage = add_message(message_b, State),

	?assertEqual(false, StateAfterAddMessage#state.connection_can_send),
	?assertEqual([], StateAfterAddMessage#state.messages),

	receive
		{messages, [message_b, message_a]} -> ?assert(true)
	after
		100 -> ?assert(false)
	end.

add_message_canAlwaysSendIfConnectionActive_test() ->
	State = #state{
		connection = self(),
		connection_active = true,
		connection_can_send = true,
		messages = [message_a]
	},
	StateAfterAddMessage = add_message(message_b, State),

	?assertEqual(true, StateAfterAddMessage#state.connection_can_send),
	?assertEqual([], StateAfterAddMessage#state.messages),

	receive
		{messages, [message_b, message_a]} -> ?assert(true)
	after
		100 -> ?assert(false)
	end.

-endif.