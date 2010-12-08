#!/usr/bin/env escript
%%! -noinput -pa ../rogueunlike/ebin -pa ../rogueunlike/deps/encurses/ebin -sname ru -setcookie ru
-include_lib("rogueunlike/include/encurses.hrl").
-include_lib("rogueunlike/include/ru.hrl").
main(_) -> ru:go().
