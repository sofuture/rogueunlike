all: deps
	@./rebar compile

deps:
	@./rebar get-deps

test: force
	@./rebar eunit

clean-all: clean clean-deps

clean:
	@./rebar clean

clean-deps:
	@rm -rf deps

run: all
	@./run.escript

force: ;
