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

-define(PERIOD, 1000).

%%% =================================================================================== %%%
%%% CALLBACKS                                                                           %%%
%%% =================================================================================== %%%

init(_Transport, Req, _Opts, _Active) ->
	io:format("bullet init~n"),
	{ok, Req, undefined}.

stream(Data, Req, State) ->
	io:format("stream received ~s~n", [Data]),
	NewState = handle_json(sdd_json:decode(Data), State),
	{ok, Req, NewState}.

info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.

%%% =================================================================================== %%%
%%% PRIVATE HELPER CALLBACK                                                             %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Find or create the client process with the given id, connect to it and return its pid

handle_json([<<"hello">>, ClientId], undefined) ->
	sdd_client:add_connection(ClientId, self()).

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

handle_json_hello_addsConnectionToClient_test() ->
	meck:new(sdd_client),
	meck:expect(sdd_client, add_connection, fun
		(<<"ClientA">>, _ConnectionId) -> "ClientAPid"
	end),

	ClientPid = handle_json([<<"hello">>, <<"ClientA">>], undefined),

	?assert(meck:called(sdd_client, add_connection, [<<"ClientA">>, self()])),
	?assertEqual(ClientPid, "ClientAPid"),

	?assert(meck:validate(sdd_client)),
	meck:unload(sdd_client).

-endif.