%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%%

-module(rogueunlike_menu).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").

-export([draw/0, get_choice/0]).

draw() ->
    cecho:curs_set(?ceCURS_INVISIBLE),

    %% do some math to size our window
    {MY, MX} = cecho:getmaxyx(),
    MenuWidth = menu_text_width() + 2,
    MenuHeight = menu_text_height() + 2,
    StartX = (MX - MenuWidth) div 2,
    StartY = (MY - MenuHeight) div 2,
    
    %% create our window
    Win = cecho:newwin(MenuHeight, MenuWidth, StartY, StartX),
    cecho:wborder(Win, $|, $|, $-, $-, $+, $+, $+, $+),
    cecho:wrefresh(Win),

    %% print some diagnostics
    cecho:move(0,1),
    cecho:addstr(io_lib:format("Window (~p,~p) Menu (~p,~p) Start(~p,~p)",
            [MX,MY,MenuWidth, MenuHeight, StartX, StartY])),

    %% print the menu items
    Print = fun(Elem) ->
        {N, Text} = Elem,
        ItemStr = io_lib:format("~p - ~s", [N, Text]),
        cecho:mvwaddstr(Win, N, 1, ItemStr)
    end,
    lists:foreach(Print, menu_items()),
    cecho:wrefresh(Win),
    cecho:refresh(),

    %% clean up
    cecho:curs_set(?ceCURS_NORMAL),
    Win.

%% read input until newline

getline() ->
    case cecho:getch() of
        $\n -> [];
        Char -> getline([Char])
    end.

getline(Buffer) ->
    case cecho:getch() of 
        $\n -> lists:reverse(Buffer);
        NextChar -> getline([NextChar | Buffer])
    end.

%% get menu choice

get_choice() ->
    getline().

menu_text_height() ->
    length(menu_items()).

menu_text_width() ->
    Items = menu_items(),
    MaxLen = fun(Elem, Max) ->
        {N, Text} = Elem,
        LenText = length(Text) + 3 + (N div 10) + 1,
        case LenText > Max of
            true -> LenText;
            false -> Max
        end
    end,
    lists:foldl(MaxLen, 0, Items).

%% get menu items

menu_items() ->
    [{1, "Choose Something"},
        {2, "Please"},
        {3, "Something else ineffective"},
        {4, "Exit"}].

