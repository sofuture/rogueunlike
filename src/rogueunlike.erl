%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(rogueunlike).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-behaviour(application).
-include("cecho.hrl").
-include("rogueunlike.hrl").

-export([start/2,stop/1]).

-export([go/0,die/0]).

%% ============================================================================
%% Behaviour Callbacks
%% ============================================================================

start(_,_) ->
    cecho_srv:start_link().

stop(_) ->
    ok.

%% ============================================================================
%% Application API
%% ============================================================================

go() ->
    init(),

    cecho:cbreak(),
    
    MenuItems = menu_items(),
    OtherItems = other_items(),

    Menu1 = rogueunlike_menu:draw(MenuItems),
    timer:sleep(5000),
    cecho:delwin(Menu1),
    cecho:refresh(),
    cecho:wrefresh(Menu1),

    Menu2 = rogueunlike_menu:draw(OtherItems),
    timer:sleep(5000),
    cecho:delwin(Menu2),
    cecho:refresh(),

    die().

die() ->
    application:stop(rogueunlike),
    halt().

%% ============================================================================
%% Internal Functions
%% ============================================================================

init() ->
    application:start(rogueunlike),
    ok.

menu_items() ->
    [{1, "Choose Something"},
        {2, "Please"},
        {3, "Something else ineffective"},
        {4, "Exit"}].

other_items() ->
    [{1, "BüNayiramdaqu Dumdadu Arad Ulus"},
        {2, "Juzur al Qamar . Jumh.r.yat al Qamar al Mutta.idah"}].

