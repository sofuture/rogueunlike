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

-include("ru.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([start_link/0]).
-export([move/2, open_door/1, close_door/1, add_mob/3, add_hero/1, attack/2]).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_mob(Type, Location, Func) ->
    ?CALL({add, Type, Location, Func}).

add_hero(Location) ->
    ?CALL({add, hero, Location}).

move(Who, Direction) ->
    ?CALL({move, Who, Direction}).

open_door(Direction) ->
    ?CALL({open_door, Direction}).

close_door(Direction) ->
    ?CALL({close_door, Direction}).

attack(Who, Direction) ->
    ?CALL({attack, Who, Direction}).

%% ============================================================================
%% Application Behavior
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call({add, hero, Location}, _From, State) ->
    {reply, do_add_hero(Location), State};
handle_call({add, Type, Location, Func}, _From, State) ->
    {reply, do_add_mob(Type, Location, Func), State};
handle_call({move, hero, Direction}, _From, State) ->
    {reply, do_move(hero, Direction), State};
handle_call({move, WhoRef, Direction}, _From, State) ->
    {reply, do_move(WhoRef, Direction), State};
handle_call({open_door, Direction}, _From, State) ->
    {reply, do_open_door(Direction), State};
handle_call({close_door, Direction}, _From, State) ->
    {reply, do_close_door(Direction), State};
handle_call({attack, WhoRef, Direction}, _From, State) ->
    {reply, do_attack(WhoRef, Direction), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
                    case ?HAS(Square, mob) of
                        true ->
                            do_attack(hero, Direction);
                        false ->
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

do_attack(hero, Direction) ->
    do_rest_of_attack(hero, ru_world:hero_location(), Direction);
do_attack(WhoRef, Direction) ->
    do_rest_of_attack(WhoRef, ru_world:mob_location(WhoRef), Direction).

do_rest_of_attack(Who, Current, Direction) ->
    case Current of
        nil -> ok;
        _ ->
            {DX, DY} = ru_util:direction_coords(Current#world.loc, Direction),
            Square = ?GET({DX,DY}),
            FindMob = fun(Elem) -> is_record(Elem, mob) end,
            Mobs = lists:filter(FindMob, Square#world.stuff),
            Ret = case Mobs of
                [Mob] ->
                    Next = Mob#mob { attackedby = Who },
                    ru_mobs:update(Next),
                    ru_mobs:attack(Next),
                    ?MSG("MOB THERE"),
                    ok;
                [] ->
                    ?MSG("No mob there"),
                    nomob
            end,
            ru:redraw(attack),
            Ret
    end.
