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
-export([new/1, state/1, past/1, append/3, add_listener/4, remove_listener/2]).

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
%% Returns past events as a list

past(History) ->
	History#history.past.

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

new_assignsRealizerFunction_test() ->
	?assertMatch(#history{realizer_function = dummy}, new(dummy)).

state_returnsState_test() ->
	?assertEqual(dummy, state(#history{state = dummy})).

past_returnsPast_test() ->
	?assertEqual(dummy, past(#history{past = dummy})).

add_listener_addsListener_test() ->
	?assertMatch(#history{listeners = [dummy]}, add_listener(#history{}, dummy)).

-endif.