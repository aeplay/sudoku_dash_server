-define(history_assert_states_equal(History1, History2),
	?assertEqual(sdd_history:state(History1), sdd_history:state(History2))
).

-define(history_assert_state_field_equals(History, Field, ExpectedValue),
	fun() ->
		State = sdd_history:state(History),
		?assertEqual(ExpectedValue, State#state.Field)
	end ()
).

-define(history_assert_past_matches(History, ExpectedMatch),
	fun() ->
		Past = sdd_history:past(History),
		?assertMatch(ExpectedMatch, Past)
	end ()
).