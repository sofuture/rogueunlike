%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(rogueunlike_char).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("rogueunlike.hrl").

-export([char_loop/1, stat_line/1]).

%% ============================================================================
%% Module API
%% ============================================================================

char_loop(Char) ->
    receive
%        {redraw, _Reason} ->
%            console ! {stats, Char},
%            char_loop(Char);

        {stats} ->
            console ! {stats, Char},
            char_loop(Char);

        {char, NewChar} ->
            char_loop(NewChar);

        {exit, _} -> 
            ok;

        _ -> 
            char_loop(Char)
    end.

stat_line(Char) ->
    Name = Char#cstats.name,
    Race = Char#cstats.race,
    Level = Char#cstats.level,
    Hp = Char#cstats.hp,
    HpMax = Char#cstats.hpmax,
    Format = "~s the ~s (Lvl ~p) HP: ~p/~p",
    io_lib:format(Format, [Name, Race, Level, Hp, HpMax]).

%% ============================================================================
%% Internal Functions
%% ============================================================================

