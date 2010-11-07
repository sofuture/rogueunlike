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

-module(ru_state).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("ru.hrl").

-export([start/0, state_loop/1]).
-export([move/2, open_door/1, close_door/1, add_mob/3, add_hero/1]).

%% ============================================================================
%% Module API
%% ============================================================================

add_mob(Type, Location, Func) ->
    ?MODULE ! {add, self(), Type, Location, Func},
    ?WAITFOROK.

add_hero(Location) ->
    ?MODULE ! {add, self(), hero, Location},
    ?WAITFOROK.

move(Ref, Direction) when is_reference(Ref) ->
    ?MODULE ! {move, self(), Ref, Direction},
    ?WAITFOROK;
move(hero, Direction) ->
    ?MODULE ! {move, self(), hero, Direction},
    ?WAITFOROK.

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
        {add, Caller, hero, Location} ->
            Caller ! do_add_hero(Location),
            state_loop(State);

        {add, Caller, Type, Location, Func} ->
            Caller ! do_add_mob(Type, Location, Func),
            state_loop(State);

        {move, Caller, hero, Direction} ->
            Caller ! do_move(hero, Direction),
            state_loop(State);

        {move, Caller, Ref, Direction} ->
            Caller ! do_move(Ref, Direction),
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

do_add_hero(Location) ->
    Square = ?GET(Location),
    ?SAVE(?ADD(Square, hero)),
    ok.

do_add_mob(Type, Location, Func) ->
    Square = ?GET(Location),
    Ref = erlang:make_ref(),
    Mob = #mob{type=Type, ref=Ref, func=Func}, 
    ru_mobs:add(Mob),
    ?SAVE(?ADD(Square, Mob)),
    ok.

do_move(Ref, Direction) when is_reference(Ref) ->
    case ru_world:mob_location(Ref) of
        {Current, Mob} ->
            {DX, DY} = ru_util:direction_coords(Current#world.loc, Direction),
            Square = ?GET({DX,DY}),
            Res = case ?HAS(Square, walkable) of
                true -> 
                    ?SAVE(?ADD(Square, Mob)),
                    ?SAVE(?SUB(Current, Mob)),
                    ok;
                false ->
                    error
            end,
            ru:redraw(move),
            Res;
        nil -> 
            ok
    end;
do_move(hero, Direction) ->
    Current = ru_world:hero_location(),
    case Current of
        nil -> ok;
        _ ->
            {DX, DY} = ru_util:direction_coords(Current#world.loc, Direction),
            Square = ?GET({DX,DY}),
            case Square of
                nil -> error;
                _ -> 
                    Res = case ?HAS(Square, walkable) of
                        true -> 
                            ?SAVE(?ADD(Square, hero)),
                            ?SAVE(?SUB(Current, hero)),
                            ok;
                        false ->
                            error
                    end,
                    ru:redraw(move),
                    Res
            end
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

