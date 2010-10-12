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
    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:move(0,0),
    cecho:addstr("Countdown: "),
    cecho:refresh(),
    cecho:curs_set(?ceCURS_NORMAL),
    %Win = cecho:newwin(20,20,0,0),
    %cecho:waddstr(Win, "Rofl"),
    %cecho:wborder(Win, "|", "|", "-", "-", "+", "+", "+", "+"),
    %timer:sleep(2000),
    %application:stop(cecho),
    %halt().a
    ok.


%% read input until newline

getline() ->
    case cecho:getch() of
        $\n -> [];
        Char -> getline([Char])
    end.

getline(Buffer) ->
    case cecho:getch() of 
        $\n -> Buffer;
        NextChar -> getline(Buffer ++ [NextChar])
    end.

%% get menu choice

get_choice() ->
    getline().

%% get menu items

menu_items() ->
    [{1, "Choose Something"},
        {2, "Please"},
        {3, "Something else ineffective"},
        {4, "Exit"}].

