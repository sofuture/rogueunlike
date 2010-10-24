%% ============================================================================
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
-export([draw/1, get_choice/0, start/0, console_loop/1]).

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
            cecho:werase(Cons#console_state.win),
            cecho:endwin(),
            cecho:initscr(),
            cecho:erase(),
            cecho:refresh(),
            Win = create_console(Cons#console_state.height),
            {_, MaxX} = cecho:getmaxyx(),
            NewCons = Cons#console_state{width=MaxX, win=Win},
            draw_console(NewCons),
            char ! {stats},
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

draw(MenuItems) ->
    cecho:curs_set(?ceCURS_INVISIBLE),
    {_MaxY, _MaxX} = cecho:getmaxyx(),

    {MenuHeight, MenuWidth, StartY, StartX} = menu_coords(MenuItems),

    %% create our window
    Win = cecho:newwin(MenuHeight, MenuWidth, StartY, StartX),
    cecho:wborder(Win, ?WINDOW_BORDERS),
    cecho:wrefresh(Win),

    %% print some diagnostics
    cecho:move(0,1),
    cecho:addstr(io_lib:format("Window (~p,~p) Menu (~p,~p) Start (~p,~p)",
            [_MaxX, _MaxY, MenuWidth, MenuHeight, StartX, StartY])),

    %% print the menu items
    Print = fun(Elem) ->
        {N, Text} = Elem,
        ItemStr = io_lib:format("~p - ~s", [N, Text]),
        cecho:mvwaddstr(Win, N, 1, ItemStr)
    end,
    lists:foreach(Print, MenuItems),
    cecho:wrefresh(Win),
    cecho:refresh(),

    %% clean up
    cecho:curs_set(?ceCURS_NORMAL),
    Win.

%% get menu choice

get_choice() ->
    cecho:getline().

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

menu_coords(MenuItems) ->
    MenuWidth = menu_width(MenuItems),
    MenuHeight = menu_height(MenuItems),
    {StartX, StartY} = ru_util:centering_coords(MenuWidth, MenuHeight),
    {MenuHeight, MenuWidth, StartY, StartX}.

menu_height(Items) ->
    %% 2 spaces for borders
    length(Items) + 2.

menu_width(Items) ->
    MaxLen = fun(Elem, Max) ->
        {N, Text} = Elem,
        %% this seems crazy but it's not (I SWEAR!!)
        %%  X spaces for text length
        %%  3 spaces for " - "
        %%  X spaces for digit ("9" -> 1, "10" -> 2)
        %%  1 space for rounding
        LenText = length(Text) + 3 + (N div 10) + 1,
        case LenText > Max of
            true -> LenText;
            false -> Max
        end
    end,
    %% 2 spaces for borders
    lists:foldl(MaxLen, 0, Items) + 2.

