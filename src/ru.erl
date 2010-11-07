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

-module(ru).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-behaviour(application).
-include("cecho.hrl").
-include("ru.hrl").

-export([start/2, stop/1]).
-export([go/0, die/0]).
-export([resize_loop/0, exit/1, redraw/1, tick/0]).

-record(state, {turn=0}).

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
    ConsoleHeight = 6,
    ru_console:create(ConsoleHeight),
    ?MSG("Press Q to quit!"),
    ru_input:set_mode({ru_input, game_mode}),
    ru_world:init(ConsoleHeight),
    ru_world:database_test(),
    ru_state:add_hero({1,1}),
    make_a_dog(),
    make_zombies(),
    ru_world:redraw(init),
    main_loop(#state{}).

die() ->
    ru_console:exit(die),
    ru_char:exit(die),
    ru_input:exit(die),
    application:stop(rogueunlike),
    halt().

%% ============================================================================
%% Internal Functions
%% ============================================================================

make_a_dog() ->
    ru_state:add_mob(dog, {2,1}, fun ru_brains:dog_brain/2).

make_zombies() ->
    ru_state:add_mob(zombie, {15,5}, fun ru_brains:zombie_brain/2),
    ru_state:add_mob(zombie, {15,6}, fun ru_brains:zombie_brain/2),
    ru_state:add_mob(zombie, {19,5}, fun ru_brains:zombie_brain/2).

main_loop(State) ->
    receive
        {tick, _} ->
            ?MSG(io_lib:format("Turn ~p", [State#state.turn+1])),
            ru_mobs:tick(),
            ru_world:tick(),
            main_loop(State#state{ turn=State#state.turn + 1});

        {redraw, Reason} ->
            % only do the whole reinit curses thing on screen resize
            case Reason of 
                sigwinch ->
                    cecho:endwin(),
                    cecho:initscr(),
                    cecho:erase(),
                    cecho:refresh(),
                    %% this is wonky, but looks much, much nicer
                    ?MODULE ! {redraw, post_sigwinch}; 
                _ -> ok
            end,
            ru_input:redraw(Reason),
            ru_world:redraw(Reason),
            ru_console:redraw(Reason),
            main_loop(State);

        {exit, _} ->
            die();

        _ -> 
            main_loop(State)
    end.

tick() ->
    ?MODULE ! {tick, tock}.

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
    ru_state:start(),
    ru_mobs:start(),
    start_self().

start_self() ->
    spawn(?MODULE, resize_loop, []),
    true = register(?MODULE, self()),
    ok.

