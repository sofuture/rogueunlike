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

-export([exit/1, draw_stats/0, set_char/1, start/0, char_loop/1]).
-export([add_item/1, remove_item/1]).
-export([stat_line/1, attr_line/1]).

%% ============================================================================
%% Module API
%% ============================================================================

draw_stats() ->
    ?MODULE ! {stats}.

set_char(Char) ->
    ?MODULE ! {char, Char}.

exit(Reason) ->
    ?MODULE ! {die, Reason}.

add_item(Item) ->
    ?MODULE ! {add, self(), Item},
    ?WAITFORRET.

remove_item(Item) ->
    ?MODULE ! {remove, self(), Item},
    ?WAITFORRET.

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

        {add, Caller, Item} ->
            {NewChar, Ret} = do_add_item(Char, Item),
            Caller ! Ret,
            char_loop(NewChar);
        
        {remove, Caller, Item} ->
            {NewChar, Ret} = do_remove_item(Char, Item),
            Caller ! Ret,
            char_loop(NewChar);

        {exit, _} -> 
            ok;

        _ -> 
            char_loop(Char)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

attr_line(Char) ->
    Att = Char#cstats.attributes,
    Str = Att#cattributes.strength,
    Dex = Att#cattributes.dexterity,
    Con = Att#cattributes.constitution,
    Int = Att#cattributes.intelligence,
    Wis = Att#cattributes.wisdom,
    Cha = Att#cattributes.charisma,
    Format = "Str: ~p Dex: ~p Con: ~p Int: ~p Wis: ~p Cha: ~p",
    io_lib:format(Format, [Str, Dex, Con, Int, Wis, Cha]).


stat_line(Char) ->
    Name = Char#cstats.name,
    Race = Char#cstats.race,
    Level = Char#cstats.level,
    Hp = Char#cstats.hp,
    HpMax = Char#cstats.hpmax,
    Format = "~s the ~s (Lvl ~p) HP: ~p/~p",
    io_lib:format(Format, [Name, Race, Level, Hp, HpMax]).

do_add_item(Char, Item) ->
    {Char#cstats{ inventory = [Item | Char#cstats.inventory] }, ok}.

do_remove_item(Char, Item) ->
    Inv = Char#cstats.inventory,
    case lists:member(Item, Inv) of
        true ->
            NotItem = fun(Elem) -> Elem =/= Item end,
            {Char#cstats{ inventory = 
                    lists:filter(NotItem, Char#cstats.inventory) }, ok};
        _ -> {Char, notfound}
    end.
            

