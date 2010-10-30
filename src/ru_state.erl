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

-export([start/0, state_loop/1]).
-export([move_hero/1, open_door/1, close_door/1]).

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
        nodoor -> nodoor;
        _ -> error
    end.

close_door(Direction) ->
    ?MODULE ! {close_door, self(), Direction},
    receive 
        ok -> ok;
        nodoor -> nodoor;
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
        
        {close_door, Caller, Direction} ->
            Caller ! do_close_door(Direction),
            state_loop(State);

        {exit, _} -> 
            ok;

        _ -> 
            state_loop(State)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

-define(HAS(A,B), ru_world:square_has(A,B)).
-define(ADD(A,B), ru_world:square_add(A,B)).
-define(SUB(A,B), ru_world:square_sub(A,B)).
-define(SAVE(A), ru_world:save_square(A)).
-define(GET(A), ru_world:get_square(A)).
-define(GET(A,B), ru_world:get_square({A,B})).

do_move_hero(Direction) ->
    Current = ru_world:hero_location(),
    case Current of
        nil -> ok;
        _ ->
            {DX, DY} = ru_util:direction_coords(Current#world.loc, Direction),
            Square = ?GET({DX,DY}),
            case ?HAS(Square, walkable) of
                true -> 
                    ?SAVE(?ADD(Square, hero)),
                    ?SAVE(?SUB(Current, hero));
                false ->
                    ok
            end,
            ru:redraw(move),
            ok
    end.

do_open_door(Direction) ->
    Current = ru_world:hero_location(),
    case Current of
        nil -> ok;
        _ ->
            {DX, DY} = ru_util:direction_coords(Current#world.loc, Direction),
            Square = ?GET({DX,DY}),
            Ret = case ?HAS(Square, door) of
                true ->
                    ?SAVE(?ADD(?SUB(Square, door), [walkable, opendoor])),
                    ok;
                false ->
                    nodoor
            end,
            ru:redraw(move),
            Ret
    end.

do_close_door(Direction) ->
    Current = ru_world:hero_location(),
    case Current of
        nil -> ok;
        _ ->
            {DX, DY} = ru_util:direction_coords(Current#world.loc, Direction),
            Square = ?GET({DX,DY}),
            Ret = case ?HAS(Square, opendoor) of
                true ->
                    ?SAVE(?ADD(?SUB(Square, [walkable, opendoor]), door)),
                    ok;
                false ->
                    nodoor
            end,
            ru:redraw(move),
            Ret
    end.

