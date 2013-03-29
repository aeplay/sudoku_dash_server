-module(sdd).

%% API.
-export([start/0, setup_persistence/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(mnesia),
	ok = application:start(sdd).

setup_persistence() ->
	application:set_env(mnesia, dir, "./db"),
	mnesia:create_schema([node()]),
	ok = application:start(mnesia),
	sdd_history:setup_persistence(player_history).