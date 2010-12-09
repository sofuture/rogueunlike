#!/usr/bin/env escript
%%! -noinput -pa ../rogueunlike/ebin -pa ../rogueunlike/deps/encurses/ebin
-include_lib("rogueunlike/deps/encurses/include/encurses.hrl").
-include_lib("rogueunlike/include/ru.hrl").
main(_) -> ru:go(), ru:start().
