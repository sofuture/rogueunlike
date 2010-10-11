-module(cecho_menu).

-compile(export_all).

-include("cecho.hrl").

menu() ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:move(1,1),
    Flag = cecho:has_colors(),
    cecho:addstr(io_lib:format("Has color: ~p",[Flag])),
    ok = print_colors(Flag),
    cecho:move(10, 10),
    cecho:addstr("Countdown: "),
    cecho:refresh(),
    cecho:curs_set(?ceCURS_NORMAL),
    timer:sleep(2000),
    application:stop(cecho),
    halt().

print_colors(false) -> ok;
print_colors(true) ->
    cecho:start_color(),
    cecho:init_pair(1, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    cecho:move(2,1),
    cecho:addstr("Colored!"),
    cecho:refresh(),
    cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    ok.

