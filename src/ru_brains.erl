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

-module(ru_brains).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("ru.hrl").

-export([dog_brain/2, zombie_brain/2]).

%% ============================================================================
%% Module API
%% ============================================================================

dog_brain(Event, Me) ->
    case Event of
        attack ->
            {MyLoc, _} = ru_world:mob_location(Me#mob.ref),
            HeroLoc = ru_world:hero_location(),
            {CX,CY} = MyLoc#world.loc,
            Dir = random_direction(),
            case ru_state:move(Me#mob.ref, Dir) of
                error -> 
                    ru_state:move(Me#mob.ref, random_direction());
                ok -> ok
            end;
        tick ->
            {MyLoc, _} = ru_world:mob_location(Me#mob.ref),
            HeroLoc = ru_world:hero_location(),
            {CX,CY} = MyLoc#world.loc,
            {HX,HY} = HeroLoc#world.loc,
            Distance = distance_between({CX,CY}, {HX,HY}),
            if
                %% if hero is more than 3 away, move towards
                Distance >= 3 ->
                    DX = case HX - CX of
                        0 -> 0; A when A >= 1 -> 1; A when A =< -1 -> -1
                    end,
                    DY = case HY - CY of
                        0 -> 0; B when B >= 1 -> 1; B when B =< -1 -> -1
                    end,
                    Dir = ru_util:coordinate_delta_direction({DX,DY});

                %% meander if close to hero
                Distance < 3 ->
                    Dir = random_direction()
            end,
            %% dont move on top of hero's square
            case ru_util:direction_coords({CX,CY}, Dir) of
                {HX, HY} -> ru_state:move(Me#mob.ref, random_direction(Dir));
                _ ->
                    case ru_state:move(Me#mob.ref, Dir) of
                        error -> 
                            ru_state:move(Me#mob.ref, random_direction());
                        ok -> ok
                    end
            end;
        _ -> ok
    end,
    ru:redraw(brain).

zombie_brain(Event, Me) ->
    case Event of 
        tick ->
            random:seed(now()),
            {MyLoc, _} = ru_world:mob_location(Me#mob.ref),
            HeroLoc = ru_world:hero_location(),
            {CX,CY} = MyLoc#world.loc,
            {HX,HY} = HeroLoc#world.loc,
            Distance = distance_between({CX,CY}, {HX,HY}),
            if
                %% if hero is less than 7 away, move towards
                Distance < 7 ->
                    DX = case HX - CX of
                        0 -> 0; A when A >= 1 -> 1; A when A =< -1 -> -1
                    end,
                    DY = case HY - CY of
                        0 -> 0; B when B >= 1 -> 1; B when B =< -1 -> -1
                    end,
                    Dir = ru_util:coordinate_delta_direction({DX,DY}),
                    DoMove = true;

                %% meander if close to hero
                Distance >= 7 ->
                    case random:uniform(2) of
                        1 -> 
                            DoMove = false,
                            Dir = nil;
                        2 -> 
                            DoMove = true,
                            Dir = random_direction()
                    end
            end,
            %% dont move on top of hero's square
            case DoMove of
                false -> ok;
                true ->
                    case ru_util:direction_coords({CX,CY}, Dir) of
                        {HX, HY} -> ru_state:move(Me#mob.ref, random_direction(Dir));
                        _ ->
                            case ru_state:move(Me#mob.ref, Dir) of
                                error -> 
                                    ru_state:move(Me#mob.ref, random_direction());
                                ok -> ok
                            end
                    end
            end;
        _ -> ok
    end,
    ru:redraw(brain).

%% ============================================================================
%% Internal Functions
%% ============================================================================

random_direction() ->
    random:seed(now()),
    Dirs = [kp_n, kp_s, kp_e, kp_w, kp_sw, kp_nw, kp_se, kp_ne],
    lists:nth(random:uniform(length(Dirs)), Dirs).

random_direction(Not) ->
    random:seed(now()),
    Dirs = lists:filter(fun(Elem) -> Elem =/= Not end,
        [kp_n, kp_s, kp_e, kp_w, kp_sw, kp_nw, kp_se, kp_ne]),
    lists:nth(random:uniform(length(Dirs)), Dirs).

distance_between({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2-X1, 2) + math:pow(Y2-Y1, 2)).

