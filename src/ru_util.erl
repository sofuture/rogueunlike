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

-module(ru_util).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("ru.hrl").

-export([get_window_dimensions/0, centering_coords/2, direction_coords/2]).
-export([coordinate_delta_direction/1]).

%% ============================================================================
%% Module API
%% ============================================================================

get_window_dimensions() ->
    {Y, X} = cecho:getmaxyx(),
    {X, Y}.

centering_coords(Width, Height) ->
    {MaxX, MaxY} = get_window_dimensions(),
    X = (MaxX - Width) div 2,
    Y = (MaxY - Height) div 2,
    {X, Y}.

coordinate_delta_direction({X, Y}) ->
    case {X,Y} of
        {0, -1} -> kp_n;
        {0, 1} -> kp_s;
        {1, 0} -> kp_e;
        {-1, 0} -> kp_w;
        {-1, -1} -> kp_nw;
        {1, -1} -> kp_ne;
        {-1, 1} -> kp_sw;
        {1, 1} -> kp_se;
        {0, 0} -> kp_center
    end.

direction_coords({X, Y} = _Location, Direction) ->
    case Direction of
        kp_n -> {X, Y-1};
        kp_s -> {X, Y+1};
        kp_e -> {X+1, Y};
        kp_w -> {X-1, Y};
        kp_nw -> {X-1, Y-1};
        kp_ne -> {X+1, Y-1};
        kp_sw -> {X-1, Y+1};
        kp_se -> {X+1, Y+1};
        kp_center -> {X, Y}
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================


