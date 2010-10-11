#!/usr/bin/env escript
%%! -noinput -pa ../rogueunlike/ebin +A 50
-include_lib("rogueunlike/include/cecho.hrl").
main(_) -> cecho_menu:menu().
