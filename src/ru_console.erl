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
            {_, MaxX} = cecho:getmaxyx(),
            console_loop(Cons#console_state{
                    win = Win, height = Height, width = MaxX});

        {stats, Char} ->
            draw_console(Cons),
            draw_stats(Char, Cons),
            console_loop(Cons);

        {redraw, _Reason} ->
            cecho:delwin(Cons#console_state.win),
            cecho:erase(),
            cecho:refresh(),
            Win = create_console(Cons#console_state.height),
            {_, MaxX} = cecho:getmaxyx(),
            NewCons = Cons#console_state{width=MaxX, win=Win},
            draw_console(NewCons),
            ru_char:draw_stats(),
            console_loop(NewCons);

        {msg, Text} ->
            NextCons = Cons#console_state{
                    lines = [Text | Cons#console_state.lines]},
            draw_console(NextCons),
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
    cecho:curs_set(?ceCURS_INVISIBLE),
    {MaxY, MaxX} = cecho:getmaxyx(),
    Win = cecho:newwin(Height+1, MaxX, MaxY-(Height+1), 0),
    cecho:wborder(Win, ?CONSOLE_BORDERS),
    cecho:wmove(Win, 0, 3),
    cecho:waddstr(Win, " Messages "),
    cecho:wrefresh(Win),
    Win.

draw_console(#console_state{lines = Lines,
        win = Win, height = Height, width = Width} = Cons) ->
    clear_lines(Win, Height, Width),
    draw_lines(Win, Lines, Height),
    cecho:wrefresh(Win),
    Cons.

draw_lines(_, [], _) -> ok;
draw_lines(_, _, 0) -> ok;
draw_lines(Win, Lines, Height) ->
    [Last | Rest] = Lines,
    cecho:wmove(Win, Height, 0),
    cecho:waddstr(Win, Last),
    draw_lines(Win, Rest, Height - 1).

clear_lines(_, 0, _) -> ok;
clear_lines(Win, Height, Width) ->
    cecho:wmove(Win, Height, 0),
    cecho:whline(Win, $\s, Width),
    clear_lines(Win, Height-1, Width).
    
%% draw the stat line after clearing it with a baseline hline

draw_stats(Char, #console_state{
        win = Win, height = _Height, width = Width} = _Cons) ->
    case Char#cstats.name of
        nil -> 
            ok;
        _ ->
            cecho:wmove(Win, 0, 0),
            cecho:whline(Win, $=, Width),
            Line = io_lib:format("  ~s  ", [ru_char:stat_line(Char)]),
            cecho:wmove(Win, 0, 2),
            cecho:waddstr(Win, Line),
            cecho:wrefresh(Win),
            ok
    end.

