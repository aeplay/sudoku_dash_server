%%% =================================================================================== %%%
%%% Sudoku Dash Game: BULLET Handler                                                    %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is a bullet handler for a realtime connection                                  %%%
%%% =================================================================================== %%%

-module(sdd_bullet_handler).

%% Callbacks
-export([init/4, stream/3, info/3, terminate/2]).

%% Records
-record(state, {
	client_pid,
	active
}).

%%% =================================================================================== %%%
%%% CALLBACKS                                                                           %%%
%%% =================================================================================== %%%

init(_Transport, Req, _Opts, Active) ->
	io:format("bullet init~n"),
	{ok, Req, #state{active = Active}}.

stream(<<"ping">>, Req, State) ->
	case State#state.client_pid of
		undefined -> do_nothing;
		Pid -> sdd_client:heartbeat(Pid)
	end,
	{reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
	io:format("stream received ~s~n", [Data]),
	NewState = handle_json(sdd_json:decode(Data), State),
	{ok, Req, NewState}.

info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{reply, sdd_json:encode(Info), Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.

%%% =================================================================================== %%%
%%% PRIVATE HELPER CALLBACK                                                             %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Find or create the client process with the given id, connect to it and return its pid

handle_json([<<"hello">>, ClientId], State) ->
	ClientPid = sdd_client:add_connection(ClientId, self(), State#state.active),
	State#state{client_pid = ClientPid};

handle_json([<<"register">>, [{<<"name">>, Name}, {<<"id">>, PlayerId}, {<<"secret">>, Secret}], _ClientId], State) ->
	sdd_client:register(State#state.client_pid, PlayerId, Name, Secret),
	State;

handle_json([<<"login">>, [{<<"secret">>, Secret}], _ClientId], State) ->
	sdd_client:login(State#state.client_pid, Secret),
	State;

handle_json([<<"find_game">>, [], _ClientId], State) ->
	sdd_client:player_do(State#state.client_pid, find_game, whatever),
	State;

handle_json([<<"chat">>, Message, _ClientId], State) ->
	sdd_client:game_do(State#state.client_pid, chat, Message),
	State;

handle_json([<<"guess">>, Position, Number, _ClientId], State) ->
	sdd_client:game_do(State#state.client_pid, guess, {Position, Number}),
	State;

handle_json([<<"leave">>, Reason, _ClientId], State) ->
	sdd_client:player_do(State#state.client_pid, leave, Reason),
	State.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

handle_json_hello_addsConnectionToClient_test() ->
	meck:new(sdd_client),
	meck:expect(sdd_client, add_connection, fun
		(<<"ClientA">>, _ConnectionId, _ConnectionActive) -> "ClientAPid"
	end),

	State = handle_json([<<"hello">>, <<"ClientA">>], #state{active = true}),

	?assert(meck:called(sdd_client, add_connection, [<<"ClientA">>, self(), true])),
	?assertEqual(State#state.client_pid, "ClientAPid"),

	?assert(meck:validate(sdd_client)),
	meck:unload(sdd_client).

-endif.