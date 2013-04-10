-module(sdd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%% =================================================================================== %%%
%%% APPLICATION CALLBACKS                                                               %%%
%%% =================================================================================== %%%

start(_StartType, _StartArgs) ->
	Port = 2739,
	SockjsState = sockjs_handler:init_state(<<"/realtime">>, fun sdd_sockjs_handler:handle/3, undefined, []),
	Routes = [
		{'_', [
			{[<<"realtime">>, '...'], sockjs_cowboy_handler, SockjsState}
		]}
	],
	{ok, _} = cowboy:start_listener(http, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, Routes}]
	),
	io:format("SockJS running on port ~p~n", [Port]),
	sdd_sup:start_link().

stop(_State) ->
	ok.