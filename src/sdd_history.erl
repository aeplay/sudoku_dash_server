%%% =================================================================================== %%%
%%% Sudoku Dash: History                                                                %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This module manages a history of events that are logged and reduced to a state.     %%%
%%% The reduction is handled by a so called realizer function that updates the state    %%%
%%% according to new events.                                                            %%%
%%% You can also add and remove listeners for new events.                               %%%
%%% =================================================================================== %%%

-module(sdd_history).

%% API
-export([new/1, state/1, append/3, add_listener/4, remove_listener/2]).

%% Types and Records
-record(history, {
	past = [],
	state = undefined,
	realizer_function = undefined,
	listeners = dict:new()
}).


%%% =================================================================================== %%%
%%% API                                                                                 %%%
%%% =================================================================================== %%%

%% ------------------------------------------------------------------------------------- %%
%% Creates an empty history and assigns the given realizer module to it

new(RealizerFunction) ->
	#history{realizer_function = RealizerFunction}.

%% ------------------------------------------------------------------------------------- %%
%% Returns the current state

state(History) ->
	History#history.state.

%% ------------------------------------------------------------------------------------- %%
%% Appends an event to the history's past, realizes it and notifies listeners

append(History, EventType, EventData) ->
	Event = {now(), EventType, EventData},
	NewPast = [Event|History#history.past],
	RealizerFunction = History#history.realizer_function,
	NewState = RealizerFunction(History#history.state, EventType, EventData),

	lists:foreach(fun(ListenerName) ->
		ListenerFunction = dict:fetch(ListenerName, History#history.listeners),
		ListenerFunction(event, Event)
	end, dict:fetch_keys(History#history.listeners)),

	History#history{past = NewPast, state = NewState}.

%% ------------------------------------------------------------------------------------- %%
%% Adds a new listener. The listener can be synchronized by replaying the whole past for
%% the listener, or by giving the listener the current state

add_listener(History, ListenerName, ListenerFunction, SynchronizationType) ->
	case SynchronizationType of
		%replay_past ->
		%	PastInOrder = lists:reverse(History#history.past),
		%	lists:foreach(fun(Event) ->
		%		ListenerFunction(event, Event)
		%	end, PastInOrder);
		%tell_current_state ->
		%	ListenerFunction(state, History#history.state);
		none ->
			ok
	end,

	NewListeners = dict:store(ListenerName, ListenerFunction, History#history.listeners),
	History#history{listeners = NewListeners}.

%% ------------------------------------------------------------------------------------- %%
%% Removes an existing listener.

remove_listener(History, ListenerName) ->
	NewListeners = dict:erase(ListenerName, History#history.listeners),
	History#history{listeners = NewListeners}.

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
	InitialHistory = #history{realizer_function=dummy},
	?assertEqual(
		InitialHistory,
		new(dummy)
	).

state_test() ->
	History = #history{realizer_function=dummy, state=test_state},
	?assertEqual(
		test_state,
		state(History)
	).

append_and_listener_test() ->
	% appending anything fails, if the realizer function is not a function
	History0 = new(not_a_function),
	?assertError(
		{badfun, not_a_function},
		append(History0, anything, anything)
	),

	% see if realizer function gets called
	RealizerFunction = fun
		(undefined, start, FirstNumber) -> FirstNumber;
		(Number, increase_by, Increment) -> Number + Increment
	end,
	History1 = new(RealizerFunction),
	History2 = append(History1, start, 5),
	?assertEqual(
		5,
		History2#history.state
	),

	% appending fails if a listener function is not a function
	History3 = add_listener(History2, broken_listener, not_a_function, none),
	?assertError(
		{badfun, not_a_function},
		append(History3, increase_by, 3)
	),

	% see if listener function gets called
	put(times_listener_called, 0),
	ListenerFunction = fun(event, Event) ->
		?assertMatch(
			{_Time, increase_by, 7},
			Event
		),
		put(times_listener_called, get(times_listener_called) + 1)
	end,
	History4 = add_listener(History2, test_listener, ListenerFunction, none),
	History5 = append(History4, increase_by, 7),
	?assertEqual(
		1,
		get(times_listener_called)
	),

	% see if listener function is not called after it has been removed
	History6 = remove_listener(History5, test_listener),
	History7 = append(History6, increase_by, 4),
	?assertEqual(
		1,
		get(times_listener_called)
	),

	erase(times_listener_called),

	% last sanity check
	?assertEqual(
		16,
		History7#history.state
	).

-endif.