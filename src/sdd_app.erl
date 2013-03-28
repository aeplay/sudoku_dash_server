-module(sdd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%% =================================================================================== %%%
%%% APPLICATION CALLBACKS                                                               %%%
%%% =================================================================================== %%%

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/realtime", bullet_handler, [{handler, sdd_bullet_handler}]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100,
		[{port, 2739}], [{env, [{dispatch, Dispatch}]}]
	),
	sdd_sup:start_link().

stop(_State) ->
	ok.