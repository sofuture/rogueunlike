%% ============================================================================
%% Rogueunlike 0.30.0
%%
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(ru_menu).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("ru.hrl").

-export([draw/1,undraw/1]).

%% ============================================================================
%% Application API
%% ============================================================================

draw(Text) ->
    Width = menu_text_width(Text) + 2,
    Height = menu_text_height(Text) + 2,
    {CX,CY} = ru_util:centering_coords(Width, Height),
    Win = encurses:newwin(Width, Height, CX, CY),
    encurses:border(Win, ?WINDOW_BORDERS),
    Print = fun(Elem, Acc) ->
        encurses:mvwaddstr(Win, 1, Acc, Elem),
        Acc + 1
    end,
    lists:foldl(Print, 1, Text),
    encurses:refresh(Win),
    Win.

undraw(_Win) ->
    ru:redraw(menu_close),
    ok.

%% ============================================================================
%% Internal Functions
%% ============================================================================

menu_text_height(Items) ->
    length(Items).

menu_text_width(Items) ->
    MaxLen = fun(Elem, Max) ->
        LenText = length(Elem),
        case LenText > Max of
            true -> LenText;
            false -> Max
        end
    end,
    lists:foldl(MaxLen, 0, Items).

