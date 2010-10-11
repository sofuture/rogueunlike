all:
	@./rebar compile

test: force
	@./rebar eunit

clean:
	@./rebar clean

run: all
	@./run.escript

force: ;
