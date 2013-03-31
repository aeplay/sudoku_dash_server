-module(sdd).

%% API.
-export([start/0, setup_persistence/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	application:set_env(mnesia, dir, "./db"),
	ok = application:start(mnesia),
	application:start(sasl),
	ok = application:start(sdd).

setup_persistence() ->
	application:set_env(mnesia, dir, "./db"),
	mnesia:create_schema([node()]),
	application:start(mnesia),
	sdd_history:setup_persistence(player_history),
	sdd_history:setup_persistence(game_history).