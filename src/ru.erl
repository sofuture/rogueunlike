%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(ru).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-behaviour(application).
-include("cecho.hrl").
-include("ru.hrl").

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
    init(),
    start_systems(),
    ru_console:create(6),
    ru_console:msg("Press Q to quit!"),
    input ! {mode, {rogueunlike_input, game_mode}},
    world ! {init},
    world ! {database_test, go},
    world ! {redraw, init},
    main_loop().

die() ->
    ru_console:exit(die),
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
            input !  world ! {redraw, sigwinch},
            ru_console:redraw(),
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
    ru_console:start(),

    true = register(char,
        spawn(ru_char, char_loop, [])),

    true = register(input,
        spawn(ru_input, input_loop, [])),

    true = register(world,
        spawn(ru_world, world_loop, [])),

    spawn(?MODULE, resize_loop, []),

    true = register(main, self()),
    ok.


