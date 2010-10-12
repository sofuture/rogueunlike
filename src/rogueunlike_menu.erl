%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%%

-module(rogueunlike_menu).

-include("cecho.hrl").

-export([draw/0, get_choice/0]).

draw() ->
    {MX,MY} = cecho:getmaxyx(),
    MenuWidth = menu_text_width(),
    MenuHeight = menu_text_height(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    
    Win = cecho:newwin(MX,MY,0,0),
    cecho:wborder(Win, $|, $|, $-, $-, $+, $+, $+, $+),
    cecho:wrefresh(Win),

    cecho:move(1,1),
    cecho:addstr(io_lib:format("Menu width: ~p height: ~p",
            [MenuWidth, MenuHeight])),
    cecho:move(2,1),
    cecho:addstr("Welp!: "),
    cecho:refresh(),
    cecho:curs_set(?ceCURS_NORMAL),

    ok.


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

%maxlen([], Max) -> Max;
%maxlen([Head|Rest], Max) ->
%    {_N, Name} = Head,
%    case length(Name) > Max of
%        true -> maxlen(Rest, length(Name));
%        false -> maxlen(Rest, Max)
%    end.

menu_text_height() ->
    length(menu_items()).

menu_text_width() ->
    Items = menu_items(),
    MaxLen = fun(Elem, Max) ->
        {_N, Text} = Elem,
        LenText = length(Text),
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

