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
	@./run_rul

debug: all
	@gdb escript --args escript run_rul

force: ;
