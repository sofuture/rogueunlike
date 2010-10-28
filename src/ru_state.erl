%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(ru_state).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("ru.hrl").

-export([start/0, state_loop/1, move_hero/1]).

%% ============================================================================
%% Module API
%% ============================================================================


%% ============================================================================
%% Application Behavior
%% ============================================================================

start() ->
    true = register(?MODULE,
        spawn(?MODULE, state_loop, [[]])).

state_loop(State) ->
    receive
        {move, {Caller, hero, Direction}} ->
            Caller ! move_hero(Direction),
            state_loop(State);


        {exit, _} -> 
            ok;

        _ -> 
            state_loop(State)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

move_hero(Direction) ->
    ru_console:msg("moving"),
    {X, Y} = ru_world:hero_location(),
    {DX, DY} = case Direction of
        kp_n -> {X, Y-1};
        kp_s -> {X, Y+1};
        kp_e -> {X+1, Y};
        kp_w -> {X-1, Y};
        kp_nw -> {X-1, Y-1};
        kp_ne -> {X+1, Y-1};
        kp_sw -> {X-1, Y+1};
        kp_se -> {X+1, Y+1};
        kp_center -> {X, Y}
    end,
    Current = ru_world:get_square({X,Y}),
    ru_console:msg(?PP([dude_at, {X,Y}])),
    Square = ru_world:get_square({DX,DY}),
    ru_console:msg(?PP([dude_movin_to, {DX,DY}])),
    ok.
