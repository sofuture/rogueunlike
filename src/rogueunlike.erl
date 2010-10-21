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
-export([resize_loop/0]).

%% ============================================================================
%% Behaviour Callbacks
%% ============================================================================

start(_,_) ->
    cecho_srv:start_link().

stop(_) ->
    ok.

%% ============================================================================
%% Module API
%% ============================================================================

go() ->
%    error_logger:tty(false),
    init(),
    start_systems(),
    console ! {create, 2},
    console ! {msg, "Press Q to quit!"},
    input ! {mode, {rogueunlike_input, game_mode}},
    world ! {init},
    world ! {database_test, go},
    world ! {redraw, init},
    main_loop().

die() ->
    console ! {exit, die},
    char ! {exit, die},
    input ! {exit, die},
    application:stop(rogueunlike),
    halt().

%% ============================================================================
%% Internal Functions
%% ============================================================================

main_loop() ->
    receive
        redraw ->
            input ! console ! world ! {redraw, sigwinch},
            main_loop();

        {exit, _} ->
            die();
        _ -> 
            main_loop()
    end.

% listen for SIGWINCH at window resize

resize_loop() ->
    cecho:sigwinch(),
    main ! redraw,
    resize_loop().

init() ->
    application:start(rogueunlike),
    ok.

start_systems() ->
    true = register(console, 
        spawn(rogueunlike_menu, console_loop, [])),

    true = register(char,
        spawn(rogueunlike_char, char_loop, [])),

    true = register(input,
        spawn(rogueunlike_input, input_loop, [])),

    true = register(world,
        spawn(rogueunlike_world, world_loop, [])),

    spawn(?MODULE, resize_loop, []),

    true = register(main, self()),
    ok.


