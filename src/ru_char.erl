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

-include("encurses.hrl").
-include("ru.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([start_link/0]).
-export([set_char/1, add_item/1, remove_item/1, get_stat_line/0,
        get_attr_line/0, char_exists/0]).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_stat_line() ->
    gen_server:call(?MODULE, get_stats).

get_attr_line() ->
    gen_server:call(?MODULE, get_attrs).

set_char(Char) ->
    gen_server:call(?MODULE, {char, Char}).

add_item(Item) ->
    gen_server:call(?MODULE, {add_item, Item}).

remove_item(Item) ->
    gen_server:call(?MODULE, {remove_item, Item}).

char_exists() ->
    gen_server:call(?MODULE, char_exists).

%% ============================================================================
%% Application Behavior
%% ============================================================================

init([]) ->
    {ok, #cstats{}}.

handle_call(get_stats, _From, Char) ->
    {reply, {ok, stat_line(Char)}, Char};
handle_call(get_attrs, _From, Char) ->
    {reply, {ok, attr_line(Char)}, Char};
handle_call({char, NewChar}, _From, _Char) ->
    {reply, ok, NewChar};
handle_call({add_item, Item}, _From, Char) ->
    {NewChar, Ret} = do_add_item(Char, Item),
    {reply, Ret, NewChar};
handle_call({remove_item, Item}, _From, Char) ->
    {NewChar, Ret} = do_remove_item(Char, Item),
    {reply, Ret, NewChar};
handle_call(char_exists, _From, Char) ->
    {reply, Char#cstats.name =/= nil, Char}.

handle_cast(_, Char) ->
    {noreply, Char}.

handle_info(_Info, Char) ->
    {noreply, Char}.

terminate(_Reason, _Char) ->
    ok.

code_change(_OldVsn, Char, _Extra) ->
    {ok, Char}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

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

