%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%%

-module(rogueunlike_main).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").

-export([go/0,die/0]).

go() ->
    init(),
    Menu = rogueunlike_menu:draw(),

    %Choice = rogueunlike_menu:get_choice(),
    %cecho:move(5,5),
    %cecho:addstr(Choice),

    cecho:refresh(),
    timer:sleep(2000),
    die().

init() ->
    application:start(cecho),
    ok.

die() ->
    application:stop(cecho),
    halt().

