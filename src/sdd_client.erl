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
	end;

%% Forwards a client event to the connection and updates current_game if it should change

handle_call({handle_player_event, EventType, EventData}, _From, State) ->
	StateAfterForward = add_message({player_event, EventType, EventData}, State),
	NewState = case EventType of
		join ->
			{GameId, _Source} = EventData,
			StateAfterForward#state{current_game = GameId};
		leave ->
			StateAfterForward#state{current_game = undefined};
		_OtherEvent ->
			StateAfterForward
	end,
	{reply, continue_listening, NewState}.

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
	{noreply, State};

%% Syncs player state with client, updates our own current_game field

handle_cast({sync_player_state, Points, Badges, CurrentGame}, State) ->
	StateAfterForward = add_message({sync_player_state, Points, Badges, CurrentGame}, State),
	NewState = StateAfterForward#state{current_game = CurrentGame},
	{noreply, NewState}.

%%% =================================================================================== %%%
%%% UTILITY FUNCTION                                                                    %%%
%%% =================================================================================== %%%

add_message(Message, State) ->
	Messages = [Message | State#state.messages],
	case {State#state.connection, State#state.connection_can_send} of
		{undefined, _} -> State#state{messages = Messages};
		{_Connection, false} -> State#state{messages = Messages};
		{Connection, true} ->
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

sync_player_state_updatesCurrentGameAndForwardsStateToConnection_test() ->
	{noreply, StateAfterSync} = handle_cast({sync_player_state, 3, "Badges", "GameA"}, #state{}),

	?assertEqual("GameA", StateAfterSync#state.current_game),
	?assertEqual([{sync_player_state, 3, "Badges", "GameA"}], StateAfterSync#state.messages).

handle_player_event_updatesCurrentGameAndForwardsEventsToConnection_test() ->
	{reply, continue_listening, StateAfterSomeEvent} = handle_call({handle_player_event, some_type, some_data}, from, #state{}),
	?assertEqual([{player_event, some_type, some_data}], StateAfterSomeEvent#state.messages),

	{reply, continue_listening, StateAfterJoin} = handle_call({handle_player_event, join, {"GameA", random}}, from, #state{}),
	?assertEqual("GameA", StateAfterJoin#state.current_game),

	{reply, continue_listening, StateAfterLeave} = handle_call({handle_player_event, leave, fell_asleep}, from, StateAfterJoin),
	?assertEqual(undefined, StateAfterLeave#state.current_game).

add_message_failsIfNoConnection_test() ->
	State = #state{
		messages = [message_a]
	},
	StateAfterAdd = add_message(message_b, State),
	?assertEqual([message_b, message_a], StateAfterAdd#state.messages).

add_message_failsIfCantSendAnymore_test() ->
	State = #state{
		connection = "SomeConnection",
		connection_can_send = false,
		messages = [message_a]
	},
	StateAfterAdd = add_message(message_b, State),
	?assertEqual([message_b, message_a], StateAfterAdd#state.messages).

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