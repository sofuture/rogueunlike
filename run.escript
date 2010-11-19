#!/usr/bin/env escript
%%! -noinput -pa ../rogueunlike/ebin +A 50 -sname ru -setcookie ru
-include_lib("rogueunlike/include/cecho.hrl").
-include_lib("rogueunlike/include/ru.hrl").
main(_) -> ru:go().
