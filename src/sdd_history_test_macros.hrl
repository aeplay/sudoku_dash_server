-define(history_assert_states_equal(History1, History2),
	?assertEqual(sdd_history:state(History1), sdd_history:state(History2))
).

-define(history_assert_state_field_equals(History, Field, ExpectedValue),
	fun() ->
		HistoryState = sdd_history:state(History),
		?assertEqual(ExpectedValue, HistoryState#state.Field)
	end ()
).

-define(history_assert_past_matches(History, ExpectedMatch),
	fun() ->
		HistoryPast = sdd_history:past(History),
		?assertMatch(ExpectedMatch, HistoryPast)
	end ()
).