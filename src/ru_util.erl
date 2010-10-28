%% ============================================================================
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


