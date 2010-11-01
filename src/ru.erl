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
    ru_state:add_mob(dog, {2,1}, fun dog_brain/2).

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

random_direction() ->
    random:seed(now()),
    Dirs = [kp_n, kp_s, kp_e, kp_w, kp_sw, kp_nw, kp_se, kp_ne],
    lists:nth(random:uniform(length(Dirs)), Dirs).

distance_between({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2-X1, 2) + math:pow(Y2-Y1, 2)).

dog_brain(Event, Me) ->
    case Event of 
        tick ->
            MyLoc = ru_world:mob_location(Me),
            HeroLoc = ru_world:hero_location(),
            {CX,CY} = MyLoc#world.loc,
            {HX,HY} = HeroLoc#world.loc,
            Distance = distance_between({CX,CY}, {HX,HY}),
            if
                Distance >= 3 ->
                    DX = case HX - CX of
                        0 -> 0; A when A >= 1 -> 1; A when A =< -1 -> -1
                    end,
                    DY = case HY - CY of
                        0 -> 0; B when B >= 1 -> 1; B when B =< -1 -> -1
                    end,
                    Dir = ru_util:coordinate_delta_direction({DX,DY});

                Distance < 3 ->
                    Dir = random_direction()
            end,
            case ru_state:move(Me, Dir) of
                error -> 
                    ru_state:move(Me, random_direction());
                ok -> ok
            end;

        _ -> ok
    end.



