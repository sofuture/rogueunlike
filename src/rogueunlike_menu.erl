%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(rogueunlike_menu).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("rogueunlike.hrl").

-export([draw/1, get_choice/0]).

-define(WINDOW_BORDERS, $|, $|, $-, $-, $+, $+, $+, $+).

%% ============================================================================
%% Application API
%% ============================================================================

draw(MenuItems) ->
    cecho:curs_set(?ceCURS_INVISIBLE),
    {_MaxY, _MaxX} = cecho:getmaxyx(),

    {MenuHeight, MenuWidth, StartY, StartX} = menu_coords(MenuItems),

    %% create our window
    Win = cecho:newwin(MenuHeight, MenuWidth, StartY, StartX),
    cecho:wborder(Win, ?WINDOW_BORDERS),
    %cecho:wborder(Win, $|, $|, $-, $-, $+, $+, $+, $+),
    cecho:wrefresh(Win),

    %% print some diagnostics
    cecho:move(0,1),
    cecho:addstr(io_lib:format("Window (~p,~p) Menu (~p,~p) Start (~p,~p)",
            [_MaxX, _MaxY, MenuWidth, MenuHeight, StartX, StartY])),

    %% print the menu items
    Print = fun(Elem) ->
        {N, Text} = Elem,
        ItemStr = io_lib:format("~p - ~s", [N, Text]),
        cecho:mvwaddstr(Win, N, 1, ItemStr)
    end,
    lists:foreach(Print, MenuItems),
    cecho:wrefresh(Win),
    cecho:refresh(),

    %% clean up
    cecho:curs_set(?ceCURS_NORMAL),
    Win.

%% get menu choice

get_choice() ->
    cecho:getline().

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% compute required menu dimensions

menu_coords(MenuItems) ->
    {MaxY, MaxX} = cecho:getmaxyx(),
    MenuWidth = menu_width(MenuItems),
    MenuHeight = menu_height(MenuItems),
    StartX = (MaxX - MenuWidth) div 2,
    StartY = (MaxY - MenuHeight) div 2,
    {MenuHeight, MenuWidth, StartY, StartX}.

menu_height(Items) ->
    %% 2 spaces for borders
    length(Items) + 2.

menu_width(Items) ->
    MaxLen = fun(Elem, Max) ->
        {N, Text} = Elem,
        %% this seems crazy but it's not (I SWEAR!!)
        %%  X spaces for text length
        %%  3 spaces for " - "
        %%  X spaces for digit ("9" -> 1, "10" -> 2)
        %%  1 space for rounding
        LenText = length(Text) + 3 + (N div 10) + 1,
        case LenText > Max of
            true -> LenText;
            false -> Max
        end
    end,
    %% 2 spaces for borders
    lists:foldl(MaxLen, 0, Items) + 2.

