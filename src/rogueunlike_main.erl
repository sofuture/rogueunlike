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
    rogueunlike_menu:draw(),
    Choice = rogueunlike_menu:get_choice(),
    die().
    


init() ->
    application:start(cecho),
    ok.


die() ->
    application:stop(cecho),
    halt().

