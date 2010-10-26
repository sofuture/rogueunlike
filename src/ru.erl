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
-export([resize_loop/0,exit/1,redraw/1]).

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
    ru_input:set_mode({ru_input, game_mode}),
    ru_world:init(),
    ru_world:database_test(),
    ru_world:redraw(init),
    main_loop().

die() ->
    ru_console:exit(die),
    ru_char:exit(die),
    ru_input:exit(die),
    application:stop(rogueunlike),
    halt().

%% ============================================================================
%% Internal Functions
%% ============================================================================

main_loop() ->
    receive
        {redraw, Reason} ->
            ru_input:redraw(Reason),
            ru_world:redraw(Reason),
            ru_console:redraw(Reason),
            main_loop();

            %input !  world ! {redraw, sigwinch},

        {exit, _} ->
            die();
        _ -> 
            main_loop()
    end.

redraw(Reason) ->
    ?MODULE ! {redraw, Reason}.

exit(Reason) ->
    ?MODULE ! {exit, Reason}.

% listen for SIGWINCH at window resize

resize_loop() ->
    cecho:sigwinch(),
    redraw(sigwinch),
    resize_loop().

init() ->
    application:start(rogueunlike),
    ok.

start_systems() ->
    ru_console:start(),
    ru_char:start(),
    ru_input:start(),
    ru_world:start(),

%    true = register(input,
%        spawn(ru_input, input_loop, [])),

%    true = register(world,
%        spawn(ru_world, world_loop, [])),

    spawn(?MODULE, resize_loop, []),

    true = register(?MODULE, self()),
    ok.


