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
-export([new/1, state/1, past/1, append/3, add_listener/3]).

%% Types and Records
-record(history, {
	past = [],
	state = undefined,
	realizer_function = fun(State,_,_) -> State end,
	listeners = []
}).

-record(persisted_history, {
	id,
	past = [],
	state = undefined
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
	NewPast = [Event | History#history.past],

	RealizerFunction = History#history.realizer_function,
	NewState = RealizerFunction(History#history.state, EventType, EventData),

	NewListeners = lists:filter(fun(ListenerFunction) ->
		continue_listening =:= ListenerFunction(event, Event)
	end, History#history.listeners),
	
	History#history{past = NewPast, state = NewState, listeners = NewListeners}.

%% ------------------------------------------------------------------------------------- %%
%% Adds a new listener. The listener can be synchronized by replaying the whole past for
%% the listener, or by giving the listener the current state

add_listener(History, ListenerFunction, SynchronizationType) ->
	case SynchronizationType of
		replay_past ->
			PastInOrder = lists:reverse(History#history.past),
			lists:foreach(fun(Event) ->
				ListenerFunction(event, Event)
			end, PastInOrder);
		tell_state ->
			ListenerFunction(state, History#history.state);
		none ->
			ok
	end,

	NewListeners = [ListenerFunction|History#history.listeners],
	History#history{listeners = NewListeners}.

%% ------------------------------------------------------------------------------------- %%
%% Creates Mnesia table that will be needed to persist histories of a given type

setup_persistence(HistoryType) ->
	mnesia:create_table(HistoryType, [
		{attributes, record_info(fields, persisted_history)},
		{index, [id]},
		{disc_copies, [node()]}
	]).

%% ------------------------------------------------------------------------------------- %%
%% Persists a history, identified by history type and id

save_persisted(HistoryType, Id, History) ->
	PersistedHistory = #persisted_history{
		id = Id,
		state = History#history.state,
		past = History#history.past
	},
	mnesia:transaction(fun() ->
		mnesia:write(HistoryType, PersistedHistory, write)
	end).

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
	?assertMatch(#history{listeners = [dummy]}, add_listener(#history{}, dummy, none)).

add_listener_canSyncByReplayingPast_test() ->
	History = #history{past = [c,b,a]},
	ListenerFunction = fun(event, Event) ->
		self() ! Event
	end,
	add_listener(History, ListenerFunction, replay_past),

	receive a -> ?assert(true)
	after 10 -> ?assert(false)
	end,

	receive b -> ?assert(true)
	after 10 -> ?assert(false)
	end,

	receive c -> ?assert(true)
	after 10 -> ?assert(false)
	end.

add_listener_canSyncByTellingState_test() ->
	ListenerFunction = fun(state, State) ->
		self() ! State
	end,

	add_listener(#history{state = dummy}, ListenerFunction, tell_state),

	receive dummy -> ?assert(true)
	after 10 -> ?assert(false)
	end.

append_appendsEventToPast_test() ->
	?assertMatch(
		#history{past = [{_Time, e_type,e_data},dummy]},
		append(#history{past=[dummy]}, e_type, e_data)
	).

append_changesStateWithRealizerFuntion_test() ->
	RealizerFunction = fun(Number, increase, Amount) ->
		Number + Amount
	end,
	?assertMatch(
		#history{state = 5},
		append(#history{state = 1, realizer_function = RealizerFunction}, increase, 4)
	).

append_NotifiesListenersAndRemovesOnesThatAreUninterested_test() ->
	InterestedListener = fun(event, {Time, EventType, EventData}) ->
		self() ! {Time, EventType, EventData},
		continue_listening
	end,
	UninterestedListener = fun(event, _) ->
		whatever
	end,
	History = #history{listeners = [InterestedListener, UninterestedListener]},
	?assertMatch(
		#history{listeners = [InterestedListener]},
		append(History, e_type, e_data)
	),

	receive {_Time, e_type, e_data} -> ?assert(true)
	after 10 -> ?assert(false)
	end.

-define(meck_mnesia,
	meck:new(mnesia),
	meck:expect(mnesia, create_table, fun
		(_TableName, _Opts) -> {atomic, ok}
	end),
	meck:expect(mnesia, transaction, fun
		(TransactionF) ->
			Result = TransactionF(),
			{atomic, Result}
	end),
	meck:expect(mnesia, write, fun
		(_Table, _Record, write) -> ok
	end)
).

setup_persistence_createsMnesiaTables_test() ->
	?meck_mnesia,

	setup_persistence(history_type),

	?assert(meck:called(mnesia, create_table, [history_type,
		[
			{attributes, record_info(fields, persisted_history)},
			{index, [id]},
			{disc_copies, [node()]}
		]
	])),
	?assert(meck:validate(mnesia)),
	meck:unload(mnesia).

save_persisted_persistsAHistory_test() ->
	?meck_mnesia,

	save_persisted(history_type, some_id, #history{state = some_state, past = some_past}),

	?assert(meck:called(mnesia, write, [history_type, #persisted_history{id = some_id, state = some_state, past = some_past}, write])),
	?assert(meck:validate(mnesia)),
	meck:unload(mnesia).

-endif.