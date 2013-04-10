%%% =================================================================================== %%%
%%% Sudoku Dash Game: SockJS Handler                                                    %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This is a SockJS handler for a realtime connection                                  %%%
%%% =================================================================================== %%%

-module(sdd_sockjs_handler).

%% Callbacks
-export([handle/3, send/2]).

%% Records
-record(state, {
	client_pid
}).

%%% =================================================================================== %%%
%%% CALLBACKS                                                                           %%%
%%% =================================================================================== %%%

handle(_Conn, init, _State) ->
	io:format("connection opened~n"),
	{ok, #state{}};
handle(Conn, {recv, Data}, State) ->
	Json = sdd_json:decode(Data),
	io:format("received ~p~n", [Json]),
	NewState = handle_json(Json, Conn, State),
	{ok, NewState};
handle(_Conn, closed, State) ->
	sdd_client:disconnect(State#state.client_pid),
	io:format("connection closed~n"),
	{ok, State}.

send(Conn, Message) ->
	io:format("sending ~p~n", [Message]),
	Conn:send(sdd_json:encode(Message)).

%%% =================================================================================== %%%
%%% PRIVATE HELPER CALLBACK                                                             %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Find or create the client process with the given id, connect to it and return its pid

handle_json([<<"hello">>, ClientId], Conn, State) ->
	ClientPid = sdd_client:set_connection(ClientId, Conn),
	State#state{client_pid = ClientPid};

handle_json([<<"register">>, [{<<"name">>, Name}, {<<"id">>, PlayerId}, {<<"secret">>, Secret}], _ClientId], _Conn, State) ->
	sdd_client:register(State#state.client_pid, PlayerId, Name, Secret),
	State;

handle_json([<<"login">>, [{<<"secret">>, Secret}], _ClientId], _Conn, State) ->
	sdd_client:login(State#state.client_pid, Secret),
	State;

handle_json([<<"find_game">>, [], _ClientId], _Conn, State) ->
	sdd_client:player_do(State#state.client_pid, find_game, whatever),
	State;

handle_json([<<"chat">>, Message, _ClientId], _Conn, State) ->
	sdd_client:game_do(State#state.client_pid, chat, Message),
	State;

handle_json([<<"guess">>, Position, Number, _ClientId], _Conn, State) ->
	sdd_client:game_do(State#state.client_pid, guess, {Position, Number}),
	State;

handle_json([<<"leave">>, Reason, _ClientId], _Conn, State) ->
	sdd_client:player_do(State#state.client_pid, leave, Reason),
	State.