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
    start_systems(),

    console ! {create, 2},

    console ! {msg, "You are in a dark maze of twisty passages, all of them alike."},
    timer:sleep(2000),

    console ! {msg, "You hear a noise."},
    timer:sleep(2000),
    
    console ! {msg, "It is pitch black. You are likely to be eaten by a grue."},
    timer:sleep(2000),

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

start_systems() ->
    true = register(console, 
        spawn(rogueunlike_menu, console_loop, [#console_state{}])),
    true = register(char,
        spawn(rogueunlike_char, char_loop, [#cstats{}])),
    ok.


