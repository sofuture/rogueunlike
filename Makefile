all: deps
	@./rebar compile

deps:
	@./rebar get-deps

test: force
	@./rebar eunit

clean:
	@./rebar clean

clean-deps:
	@rm -rf deps

run: all
	@./run.escript

force: ;
