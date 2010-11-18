#!/usr/bin/env escript
%%! -noinput -pa ../rogueunlike/ebin +A 10
-include_lib("rogueunlike/include/cecho.hrl").
-include_lib("rogueunlike/include/ru.hrl").
main(_) -> ru:go().
