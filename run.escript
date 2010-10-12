#!/usr/bin/env escript
%%! -noinput -pa ../rogueunlike/ebin +A 50
-include_lib("rogueunlike/include/cecho.hrl").
-include_lib("rogueunlike/include/rogueunlike.hrl").
main(_) -> rogueunlike:go().
