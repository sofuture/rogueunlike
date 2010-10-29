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

-export([start/0, state_loop/1, move_hero/1, open_door/1]).

%% ============================================================================
%% Module API
%% ============================================================================

move_hero(Direction) ->
    ?MODULE ! {move_hero, self(), Direction},
    receive 
        ok -> ok;
        _ -> error
    end.

open_door(Direction) ->
    ?MODULE ! {open_door, self(), Direction},
    receive 
        ok -> ok;
        _ -> error
    end.

%% ============================================================================
%% Application Behavior
%% ============================================================================

start() ->
    true = register(?MODULE,
        spawn(?MODULE, state_loop, [[]])).

state_loop(State) ->
    receive
        {move_hero, Caller, Direction} ->
            Caller ! do_move_hero(Direction),
            state_loop(State);

        {open_door, Caller, Direction} ->
            Caller ! do_open_door(Direction),
            state_loop(State);

        {exit, _} -> 
            ok;

        _ -> 
            state_loop(State)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

do_move_hero(Direction) ->
    Current = ru_world:hero_location(),
    {DX, DY} = ru_util:direction_coords(Current#world.loc, Direction),
    Square = ru_world:get_square({DX,DY}),
    case ru_world:square_has(Square, walkable) of
        true -> 
            ru_world:save_square(ru_world:square_add(Square, hero)),
            ru_world:save_square(ru_world:square_sub(Current, hero));
        false ->
            ok
    end,
    ru:redraw(move),
    ok.

do_open_door(Direction) ->
    Current = ru_world:hero_location(),
    {DX, DY} = ru_util:direction_coords(Current#world.loc, Direction),
    Square = ru_world:get_square({DX,DY}),
    Ret = case ru_world:square_has(Square, door) of
        true -> 
            ru_world:save_square(ru_world:square_add(Square, [walkable, opendoor])),
            ok;
        false ->
            error
    end,
    ru:redraw(move),
    Ret.
