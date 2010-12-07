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

-include("encurses.hrl").
-include("ru.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([start_link/0]).
-export([create/1, redraw/1, message/1]).

-record(state, {
        win = nil,
        lines = [],
        statline = [],
        height = 0,
        width = 0}).

%% ============================================================================
%% Application API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Height) ->
    gen_server:call(?MODULE, {create, Height}).

redraw(Reason) ->
    gen_server:call(?MODULE, {redraw, Reason}).

message(Message) ->
    gen_server:call(?MODULE, {message, Message}).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init([]) ->
    {ok, #state{}}.

handle_call({create, Height}, _From, State) ->
    {reply, ok, do_create_console(State, Height)};
handle_call({redraw, _Reason}, _From, State) ->
    {reply, ok, do_redraw(State)};
handle_call({message, Message}, _From, State) ->
    {reply, ok, do_message(State, Message)};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({create, Height}, State) ->
    {noreply, do_create_console(State, Height)};
handle_cast({redraw, _Reason}, State) ->
    {noreply, do_redraw(State)};
handle_cast({message, Message}, State) ->
    {noreply, do_message(State, Message)};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

do_create_console(State, Height) ->
    Win = create_console(Height),
    {MaxX, _} = encurses:getmaxxy(),
    encurses:refresh(Win),
    State#state{ win=Win, height=Height, width=MaxX }.

do_redraw(State) ->
    encurses:delwin(State#state.win),
    encurses:erase(State#state.win),
    Win = create_console(State#state.height),
    {MaxX, _} = encurses:getmaxxy(),
    NewCons = State#state{ width=MaxX, win=Win },
    draw_console(NewCons),
    draw_stats(NewCons),
    encurses:refresh(NewCons#state.win),
    NewCons.

do_message(State, Message) ->
    NewCons = State#state{ lines = [Message | State#state.lines]},
    draw_console(NewCons),
    encurses:refresh(NewCons#state.win),
    NewCons.

%% ============================================================================
%% Internal Functions
%% ============================================================================

create_console(Height) ->
    encurses:curs_set(?CURS_INVISIBLE),
    {MaxX, MaxY} = encurses:getmaxxy(),
    Win = encurses:newwin(MaxX, Height+1, 0, MaxY-(Height+1)),
    encurses:border(Win, ?CONSOLE_BORDERS),
    encurses:mvwaddstr(Win, 3, 0, " Messages "),
    Win.

draw_console(#state{lines = Lines,
        win = Win, height = Height, width = Width} = Cons) ->
    clear_lines(Win, Height, Width),
    draw_lines(Win, Lines, Height),
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
    
draw_stats(#state{win = Win, height = _Height, width = Width} = _Cons) ->
    case ru_char:char_exists() of
        true ->
            {ok, Stats} = ru_char:get_stat_line(),
            {ok, Attrs} = ru_char:get_attr_line(),
            encurses:move(Win, 0, 0),
            encurses:hline(Win, $=, Width),
            StatLine = io_lib:format(" ~s ", [Stats]),
            encurses:mvwaddstr(Win, 2, 0, StatLine),
            AttrLine = io_lib:format(" ~s ", [Attrs]),
            AttrLineLen = length(lists:flatten(AttrLine)),
            encurses:mvwaddstr(Win, (Width-(AttrLineLen+2)), 0, AttrLine),
            ok;
        _ -> ok
    end.

