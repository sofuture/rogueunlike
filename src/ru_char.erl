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

-module(ru_char).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("ru.hrl").

-export([exit/1, draw_stats/0, set_char/1, start/0, char_loop/1, stat_line/1]).

%% ============================================================================
%% Module API
%% ============================================================================

draw_stats() ->
    ?MODULE ! {stats}.

set_char(Char) ->
    ?MODULE ! {char, Char}.

exit(Reason) ->
    ?MODULE ! {die, Reason}.

%% ============================================================================
%% Application Behavior
%% ============================================================================

start() ->
    true = register(?MODULE,
        spawn(?MODULE, char_loop, [#cstats{}])).

char_loop(Char) ->
    receive
        {stats} ->
            ru_console:char_stats(Char),
            char_loop(Char);

        {char, NewChar} ->
            ru_console:char_stats(NewChar),
            char_loop(NewChar);

        {exit, _} -> 
            ok;

        _ -> 
            char_loop(Char)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

stat_line(Char) ->
    Name = Char#cstats.name,
    Race = Char#cstats.race,
    Level = Char#cstats.level,
    Hp = Char#cstats.hp,
    HpMax = Char#cstats.hpmax,
    Format = "~s the ~s (Lvl ~p) HP: ~p/~p",
    io_lib:format(Format, [Name, Race, Level, Hp, HpMax]).

