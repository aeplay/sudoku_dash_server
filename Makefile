test:
	rebar eunit skip_deps=true

compile:
	rebar compile

dialyzer-setup:
	dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl inets ssl
	rebar clean
	rebar compile
	dialyzer --build_plt --output_plt deps_plt deps/cowboy/ebin/ deps/ranch/ebin/ deps/jsx/ebin/

dialyzer:
	dialyzer --plts ~/.dialyzer_plt deps_plt --src -Wno_opaque src

console:
	werl -pa ebin deps/*/ebin/ -name sdd@home -setcookie secret &

.PHONY: console