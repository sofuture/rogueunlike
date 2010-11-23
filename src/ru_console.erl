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

-module(ru_console).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("ru.hrl").

-export([create/1, char_stats/1, redraw/1, msg/1, exit/1]).
-export([start/0, console_loop/1]).

%% ============================================================================
%% Application API
%% ============================================================================

create(Height) ->
    ?MODULE ! {create, Height}.

char_stats(Char) ->
    ?MODULE ! {stats, Char}.

redraw(Reason) ->
    ?MODULE ! {redraw, Reason}.

msg(Message) ->
    ?MODULE ! {msg, Message}.

exit(Reason) ->
    ?MODULE ! {exit, Reason}.

%% ============================================================================
%% Application Behavior
%% ============================================================================

start() ->
    true = register(?MODULE, 
        spawn(?MODULE, console_loop, [#console_state{}])).

console_loop(Cons) ->
    receive
        {create, Height} ->
            Win = create_console(Height),
            {MaxX, _} = encurses:getmaxxy(),
            encurses:refresh(Win),
            console_loop(Cons#console_state{
                    win = Win, height = Height, width = MaxX});

        {stats, Char} ->
            draw_console(Cons),
            draw_stats(Char, Cons),
            encurses:refresh(Cons#console_state.win),
            console_loop(Cons);

        {redraw, _Reason} ->
            encurses:delwin(Cons#console_state.win),
            encurses:erase(Cons#console_state.win),
            Win = create_console(Cons#console_state.height),
            {MaxX, _} = encurses:getmaxxy(),
            NewCons = Cons#console_state{width=MaxX, win=Win},
            draw_console(NewCons),
            ru_char:draw_stats(),            
            encurses:refresh(NewCons#console_state.win),
            console_loop(NewCons);

        {msg, Text} ->
            NextCons = Cons#console_state{
                    lines = [Text | Cons#console_state.lines]},
            draw_console(NextCons),
            encurses:refresh(NextCons#console_state.win),
            console_loop(NextCons);

        {exit, _} -> 
            ok;

        _ -> 
            console_loop(Cons)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

create_console(Height) ->
    encurses:curs_set(?ceCURS_INVISIBLE),
    {MaxX, MaxY} = encurses:getmaxxy(),
    Win = encurses:newwin(MaxX, Height+1, 0, MaxY-(Height+1)),
    encurses:border(Win, ?CONSOLE_BORDERS),
    encurses:mvwaddstr(Win, 3, 0, " Messages "),
    %encurses:refresh(),
    Win.

draw_console(#console_state{lines = Lines,
        win = Win, height = Height, width = Width} = Cons) ->
    clear_lines(Win, Height, Width),
    draw_lines(Win, Lines, Height),
    %encurses:refresh(Win),
    Cons.

draw_lines(_, [], _) -> ok;
draw_lines(_, _, 0) -> ok;
draw_lines(Win, Lines, Height) ->
    [Last | Rest] = Lines,
    encurses:mvwaddstr(Win, 0, Height, Last),
    draw_lines(Win, Rest, Height - 1).

clear_lines(_, 0, _) -> ok;
clear_lines(Win, Height, Width) ->
    encurses:move(Win, 0, Height),
    encurses:hline(Win, $\s, Width),
    clear_lines(Win, Height-1, Width).
    
%% draw the stat line after clearing it with a baseline hline

draw_stats(Char, #console_state{
        win = Win, height = _Height, width = Width} = _Cons) ->
    case Char#cstats.name of
        nil -> 
            ok;
        _ ->
            encurses:move(Win, 0, 0),
            encurses:hline(Win, $=, Width),
            Line = io_lib:format("  ~s  ", [ru_char:stat_line(Char)]),
            encurses:mvwaddstr(Win, 2, 0, Line),
            %encurses:refresh(Win),
            ok
    end.

