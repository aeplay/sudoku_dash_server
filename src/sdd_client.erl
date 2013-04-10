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

%% API
-export([start_link/2, set_connection/2, other_client_connected/1, disconnect/1, register/4, login/2, sync_player_state/4,
	handle_player_event/3, player_do/3, game_do/3, handle_game_event/5]).

%% GEN_SERVER
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

%% Records
-record(state, {
	id,
	info,
	connection,
	player,
	current_game
}).

%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

start_link(ClientId, ClientInfo) ->
	gen_server:start_link({global, {client, ClientId}}, ?MODULE, {ClientId, ClientInfo}, []).

set_connection(ClientId, Connection) ->
	ClientPid = case global:whereis_name({client, ClientId}) of
		undefined ->
			{ok, Pid} = sdd_clients_sup:start_client(ClientId),
			Pid;
		Pid -> Pid
	end,
	gen_server:cast(ClientPid, {set_connection, Connection}),
	ClientPid.

other_client_connected(ClientId) ->
	gen_server:cast({global, {client, ClientId}}, other_client_connected).

disconnect(ClientPid) ->
	gen_server:cast(ClientPid, disconnect).

register(ClientPid, PlayerId, Name, Secret) ->
	gen_server:cast(ClientPid, {register, PlayerId, Name, Secret}).

login(ClientPid, Secret) ->
	gen_server:cast(ClientPid, {login, Secret}).

sync_player_state(ClientId, Points, Badges, CurrentGame) ->
	catch gen_server:call({global, {client, ClientId}}, {sync_player_state, Points, Badges, CurrentGame}).

handle_player_event(ClientId, EventType, EventData) ->
	catch gen_server:call({global, {client, ClientId}}, {handle_player_event, EventType, EventData}).

handle_game_event(ClientId, GameId, Time, EventType, EventData) ->
	gen_server:cast({global, {client, ClientId}}, {handle_game_event, GameId, Time, EventType, EventData}).

player_do(ClientPid, Action, Args) ->
	gen_server:cast(ClientPid, {player_do, Action, Args}).

game_do(ClientPid, Action, Args) ->
	gen_server:cast(ClientPid, {game_do, Action, Args}).


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
	{reply, continue_listening, NewState};

%% Syncs player state with client, updates our own current_game field

handle_call({sync_player_state, Points, Badges, CurrentGame}, _From, State) ->
	StateAfterForward = add_message({sync_player_state, Points, Badges, CurrentGame}, State),
	NewState = StateAfterForward#state{current_game = CurrentGame},
	{reply, continue_listening, NewState}.

%% ------------------------------------------------------------------------------------- %%
%% Adds a new connection, remembers its active state and resets whether we can send

handle_cast({set_connection, Connection}, State) ->
	NewState = State#state{connection = Connection},
	StateAfterHelloSent = add_message({hello, connected}, NewState),
	{noreply, StateAfterHelloSent};

handle_cast(other_client_connected, State) ->
	StateAfterSend = add_message({other_client_connected}, State),
	{stop, normal, StateAfterSend};

handle_cast(disconnect, State) ->
	{stop, normal, State};

%% Tries to register a player and connect to it

handle_cast({register, PlayerId, Name, Secret}, State) ->
	case sdd_player:register(PlayerId, Name, Secret) of
		ok ->
			StateAfterSend = add_message({register_ok}, State),
			{noreply, StateAfterSend};
		already_exists ->
			StateAfterSend = add_message({register_invalid}, State),
			{noreply, StateAfterSend}
	end;

%% Tries to authenticate with a secret and connect to the given player

handle_cast({login, Secret}, State) ->
	case sdd_player:authenticate(Secret) of
		{Name, PlayerId} -> 
			sdd_player:connect(PlayerId, State#state.id, State#state.info),
			StateAfterSend = add_message({login_ok, {Name, PlayerId}}, State),
			{noreply, StateAfterSend#state{player = PlayerId}};
		false ->
			StateAfterSend = add_message({login_invalid}, State),
			{noreply, StateAfterSend}
	end;

%% Makes the player do something on behalf of the client

handle_cast({player_do, Action, Args}, State) ->
	sdd_player:do(State#state.player, Action, Args),
	{noreply, State};

%% Makes the player do something in a game in behalf of the client

handle_cast({game_do, Action, Args}, State) ->
	case {State#state.player, State#state.current_game} of
		{undefined, _} -> do_nothing;
		{_, undefined} -> do_nothing;
		{PlayerId, GameId} -> sdd_game:do(GameId, PlayerId, Action, Args)
	end,
	{noreply, State};

%% Forwards a game event to the connection

handle_cast({handle_game_event, GameId, Time, EventType, EventData}, State) ->
	StateAfterSend = add_message({game_event, GameId, Time, EventType, EventData}, State),
	{noreply, StateAfterSend}.

%% Notify player that we terminate

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	sdd_player:disconnect(State#state.player, State#state.id),
	ok.

%% ------------------------------------------------------------------------------------- %%
%% Rest of gen_server calls

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% =================================================================================== %%%
%%% UTILITY FUNCTION                                                                    %%%
%%% =================================================================================== %%%

add_message(Message, State) ->
	sdd_sockjs_handler:send(State#state.connection, Message),
	State.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("sdd_history_test_macros.hrl").

-define(meck_sdd_player,
	meck:new(sdd_player),
	meck:expect(sdd_player, authenticate, fun
		("PeterSecret") -> {"Peter", "PeterId"};
		("BadSecret") -> false
	end),
	meck:expect(sdd_player, register, fun
		("PaulId", "Paul", _Secret) -> already_exists;
		("PetraId", "Petra", _Secret) -> ok
	end),
	meck:expect(sdd_player, connect, fun
		(_PlayerId, _ClientId, _ClientInfo) -> ok
	end),
	meck:expect(sdd_player, do, fun
		(_PlayerId, _Action, _Args) -> ok
	end)
).

-define(meck_sockjs_handler,
	meck:new(sdd_sockjs_handler),
	meck:expect(sdd_sockjs_handler, send, fun
		(_Connection, _Message)-> ok
	end)
).

init_savesClientIdAndInfo_test() ->
	{ok, InitialState} = init({"ClientId", "ClientInfo"}),
	?assertEqual("ClientId", InitialState#state.id),
	?assertEqual("ClientInfo", InitialState#state.info).

register_triesToRegisterPlayer_test() ->
	?meck_sdd_player,
	?meck_sockjs_handler,

	InitialState = #state{id = "ClientId", info = "ClientInfo", player = undefined},

	{noreply, _StateAfterBadRegister} = handle_cast({register, "PaulId", "Paul","Secret"}, InitialState),
	?assert(meck:called(sdd_player, register, ["PaulId", "Paul", "Secret"])),

	{noreply, _StateAfterGoodRegister} = handle_cast({register, "PetraId", "Petra","Secret"}, InitialState),
	?assert(meck:called(sdd_player, register, ["PetraId", "Petra", "Secret"])),

	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player),
	meck:unload(sdd_sockjs_handler).

login_authenticatesWithPlayerAndConnectsIfSuccessful_test() ->	
	?meck_sdd_player,
	?meck_sockjs_handler,

	InitialState = #state{id = "ClientId", info = "ClientInfo", player = undefined},

	{noreply, StateAfterBadLogin} = handle_cast({login, "BadSecret"}, InitialState),

	?assert(meck:called(sdd_player, authenticate, ["BadSecret"])),
	?assertNot(meck:called(sdd_player, connect, ["PeterId", "ClientId", "ClientInfo"])),
	?assertEqual(StateAfterBadLogin#state.player, undefined),

	{noreply, StateAfterGoodLogin} = handle_cast({login, "PeterSecret"}, InitialState),

	?assert(meck:called(sdd_player, authenticate, ["PeterSecret"])),
	?assert(meck:called(sdd_player, connect, ["PeterId", "ClientId", "ClientInfo"])),
	?assertEqual(StateAfterGoodLogin#state.player, "PeterId"),

	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player),
	meck:unload(sdd_sockjs_handler).

set_connection_setsNewConnection_test() ->
	?meck_sockjs_handler,
	{noreply, State} = handle_cast({set_connection, connection}, #state{}),
	?assertEqual(connection, State#state.connection),
	meck:unload(sdd_sockjs_handler).

player_do_forwardsActionToPlayer_test() ->
	?meck_sdd_player,
	?meck_sockjs_handler,
	State = #state{player = "Peter"},
	{noreply, StateAfterDo} = handle_cast({player_do, "Action", "Args"}, State),

	?assert(meck:called(sdd_player, do, ["Peter", "Action", "Args"])),
	?assertEqual(State, StateAfterDo),

	?assert(meck:validate(sdd_player)),
	meck:unload(sdd_player),
	meck:unload(sdd_sockjs_handler).

game_do_forwardsActionToGameIfWeAreInOne_test() ->
	%% nothing should be called in these, else exception
	StateWithOnlyGame = #state{current_game = "GameA"},
	{noreply, StateAfterDoWithoutPlayer} = handle_cast({game_do, "Action", "Args"}, StateWithOnlyGame),
	?assertEqual(StateWithOnlyGame, StateAfterDoWithoutPlayer),

	StateWithOnlyPlayer = #state{player = "Peter"},
	{noreply, StateAfterDoWithoutGame} = handle_cast({game_do, "Action", "Args"}, StateWithOnlyPlayer),
	?assertEqual(StateWithOnlyPlayer, StateAfterDoWithoutGame),

	meck:new(sdd_game),
	meck:expect(sdd_game, do, fun
		(_GameId, _PlayerId, _Action, _Args) -> ok
	end),

	StateWithPlayerAndGame = #state{player = "Peter", current_game = "GameA"},
	{noreply, StateAfterDoWithGame} = handle_cast({game_do, "Action", "Args"}, StateWithPlayerAndGame),

	?assert(meck:called(sdd_game, do, ["GameA", "Peter", "Action", "Args"])),
	?assert(meck:validate(sdd_game)),
	meck:unload(sdd_game),

	?assertEqual(StateWithPlayerAndGame, StateAfterDoWithGame).

sync_player_state_updatesCurrentGameAndForwardsStateToConnection_test() ->
	?meck_sockjs_handler,
	{reply, continue_listening, StateAfterSync} = handle_call({sync_player_state, 3, "Badges", "GameA"}, from, #state{}),

	?assertEqual("GameA", StateAfterSync#state.current_game),
	?assert(meck:called(sdd_sockjs_handler, send, [undefined, {sync_player_state, 3, "Badges", "GameA"}])),
	?assert(meck:validate(sdd_sockjs_handler)),
	meck:unload(sdd_sockjs_handler).

handle_player_event_updatesCurrentGameAndForwardsEventsToConnection_test() ->
	?meck_sockjs_handler,
	{reply, continue_listening, _State} = handle_call({handle_player_event, some_type, some_data}, from, #state{}),
	?assert(meck:called(sdd_sockjs_handler, send, [undefined, {player_event, some_type, some_data}])),

	{reply, continue_listening, StateAfterJoin} = handle_call({handle_player_event, join, {"GameA", random}}, from, #state{}),
	?assertEqual("GameA", StateAfterJoin#state.current_game),

	{reply, continue_listening, StateAfterLeave} = handle_call({handle_player_event, leave, fell_asleep}, from, StateAfterJoin),
	?assertEqual(undefined, StateAfterLeave#state.current_game),
	?assert(meck:validate(sdd_sockjs_handler)),
	meck:unload(sdd_sockjs_handler).

-endif.