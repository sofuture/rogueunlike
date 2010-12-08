all: deps
	@./rebar compile

deps:
	@./rebar get-deps

test: force
	@./rebar eunit

clean:
	@./rebar clean

run: all
	@./run.escript

force: ;
