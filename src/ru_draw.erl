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

-module(ru_draw).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include_lib("stdlib/include/qlc.hrl").
-include_lib("deps/encurses/include/encurses.hrl").
-include("ru.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([start_link/0]).
-export([draw/1, cleanup/0, init/0, splash/0]).

-record(state, {worldwin=nil, worldheight=nil, worldwidth=nil,
        conswin=nil, consheight=6, conswidth=nil, menuwin=nil }).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init() ->
    ?CALL(init).

splash() ->
    ?CALL(splash, infinity).

draw(Reason) ->
    ?CALL({draw, Reason}).

cleanup() ->
    ?CALL(cleanup).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(_) ->
    {ok, #state{}}.

handle_call(init, _From, State) ->
    {reply, do_init(), State};
handle_call(splash, _From, State) ->
    {reply, do_splash_screen(), State};
handle_call({draw, _Reason}, _From, State) ->
    {reply, ok, do_draw(State)};
handle_call(cleanup, _From, State) ->
    {reply, do_cleanup(), State}.

handle_cast({draw, _Reason}, State) ->
    {noreply, do_draw(State)};
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
    
do_init() ->
    encurses:initscr(),
    encurses:keypad(0, true),
    encurses:noecho(),
    ok.

do_cleanup() ->
    encurses:erase(),
    encurses:refresh(),
    encurses:endwin(),
    ok.

do_draw(State) ->
    {MaxX, MaxY} = encurses:getmaxxy(),
    {ConsWin, ConsHeight} = {State#state.conswin, State#state.consheight},
    {WorldWin, WorldHeight} = {State#state.worldwin, MaxY - ConsHeight},
    MenuWin = State#state.menuwin,
    State#state{ 
        conswidth = MaxX,
        worldwin = draw_world(WorldWin, WorldHeight, MaxX),
        conswin = draw_console(ConsWin, ConsHeight, MaxX, MaxY),
        menuwin = draw_menu(MenuWin)
    }.

draw_world(nil, Height, MaxX) ->
    Win = create_world(Height, MaxX),
    draw_world(Win, Height, MaxX);
draw_world(Win, _Height, _MaxX) ->
    encurses:erase(Win),
    do_draw_world(Win),
    encurses:refresh(Win),
    Win.

draw_console(nil, Height, MaxX, MaxY) ->
    Win = create_console(Height, MaxX, MaxY),
    draw_console(Win, Height, MaxX, MaxY);
draw_console(Win, Height, MaxX, _MaxY) ->
    encurses:erase(Win),
    draw_console(Win, Height, MaxX),
    draw_stats(Win, MaxX),
    encurses:refresh(Win),
    Win.

draw_menu(Win) ->
    case ru_menu:has_menu() of
        false ->
            case Win of 
                nil -> nil;
                _ ->
                    encurses:erase(Win),
                    encurses:delwin(Win),
                    nil
            end;
        true -> 
            draw_menu(Win, ru_menu:get_lines())
    end.

draw_menu(nil, Text) ->
    Win = create_menu(Text),
    draw_menu(Win, Text);
draw_menu(Win, Text) ->
    encurses:border(Win, ?WINDOW_BORDERS),
    Print = fun(Elem, Acc) ->
        encurses:mvwaddstr(Win, 1, Acc, Elem),
        Acc + 1
    end,
    lists:foldl(Print, 1, Text),
    encurses:refresh(Win),
    Win.

create_menu(Text) ->
    Width = menu_text_width(Text) + 2,
    Height = menu_text_height(Text) + 2,
    {CX,CY} = ru_util:centering_coords(Width, Height),
    encurses:curs_set(?CURS_INVISIBLE),
    Win = encurses:newwin(Width, Height, CX, CY),
    encurses:refresh(Win),
    Win.

create_world(Height, MaxX) ->
    encurses:curs_set(?CURS_INVISIBLE),
    Win = encurses:newwin(MaxX, Height, 0, 0),
    encurses:refresh(Win),
    Win.

create_console(Height, MaxX, MaxY) ->
    encurses:curs_set(?CURS_INVISIBLE),
    Win = encurses:newwin(MaxX, Height+1, 0, MaxY-(Height+1)),
    encurses:border(Win, ?CONSOLE_BORDERS),
    encurses:mvwaddstr(Win, 3, 0, " Messages "),
    Win.

draw_console(Win, Height, Width) ->
    clear_lines(Win, Height, Width),
    draw_lines(Win, ru_console:get_lines(), Height),
    ok.

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

draw_stats(Win, Width) ->
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

do_draw_world(Win) ->
    World = ru_world:get_squares(),
    {WorldWidth, WorldHeight} = bounding_dimensions(World),
    {DrawX, DrawY} = ru_util:centering_coords(WorldWidth, WorldHeight),
    DrawF = fun(Spot) ->
        Char = square_char(Spot#world.stuff),
        {LocX, LocY} = Spot#world.loc,
        encurses:mvwaddch(Win, DrawX+LocX, DrawY+LocY, Char)
    end,
    lists:foreach(DrawF, World),
    encurses:refresh(Win),
    ok.

bounding_dimensions(World) ->
    FXs = fun(Elem) ->
        {X, _} = Elem#world.loc,
        X
    end,
    FYs = fun(Elem) ->
        {_, Y} = Elem#world.loc,
        Y
    end,
    MaxX = lists:max(lists:map(FXs, World)),
    MaxY = lists:max(lists:map(FYs, World)),
    {MaxX + 1, MaxY + 1}.

square_char(Stuff) ->
    PriElement = fun(Elem) -> draw_pref(Elem) end,
    PrefList = lists:map(PriElement, Stuff),
    {_Prio, BestChar} = lists:min(PrefList),
    BestChar.

draw_pref(Thing) ->
    case Thing of
        
        %% mobs
        hero ->
            {0, $@};

        #mob{} = MobRec ->
            Type = MobRec#mob.type,
            case Type of
                dog ->
                    {1, $d};
                zombie ->
                    {1, $Z};
                _ ->
                    {1, $?}
            end;

        %% stuff
        fountain -> 
            {1000, $U};
        opendoor ->
            {1001, $|};

        %% infrastructureystuff
        walkable -> 
            {4000, $\s};
        door ->
            {5000, $+};

        wall_ulcorner ->
            {6000, ?ACS_ULCORNER};
        wall_urcorner ->
            {6000, ?ACS_URCORNER};
        wall_llcorner ->
            {6000, ?ACS_LLCORNER};
        wall_lrcorner ->
            {6000, ?ACS_LRCORNER};
        
        wall_cross -> 
            {6000, ?ACS_PLUS};

        wall_ttee ->
            {6000, ?ACS_TTEE};
        wall_btee ->
            {6000, ?ACS_BTEE};
        wall_ltee ->
            {6000, ?ACS_LTEE};
        wall_rtee ->
            {6000, ?ACS_RTEE};

        wall_hline ->
            {6000, ?ACS_HLINE};
        wall_vline ->
            {6000, ?ACS_VLINE};

        wall ->
            {6001, $#};
        wall_floating -> 
            {6001, $#};

        _ ->
            {10000, $\s}

    end.

menu_text_height(Items) ->
    length(Items).

menu_text_width(Items) ->
    MaxLen = fun(Elem, Max) ->
        LenText = length(Elem),
        case LenText > Max of
            true -> LenText;
            false -> Max
        end
    end,
    lists:foldl(MaxLen, 0, Items).

%% ============================================================================
%% Splash screen stuff
%% ============================================================================

spiral(X,Y, DX, DY, MinX, MinY, MaxX, MaxY, Acc) ->
    encurses:mvaddch(X,Y,?ACS_CKBOARD),
    Acc1 = case Acc of
        5 -> 
            timer:sleep(1),
            encurses:refresh(),
            0;
        _ -> Acc + 1
    end,
    if
        X =:= MaxX andalso DX =:= 1 ->
            spiral(X, Y+1, 0, 1, MinX, MinY, MaxX-1, MaxY, Acc1);
        Y =:= MaxY andalso DY =:= 1 ->
            spiral(X-1, Y, -1, 0, MinX, MinY, MaxX, MaxY-1, Acc1);
        X =:= MinX andalso DX =:= -1 ->
            spiral(X, Y-1, 0, -1, MinX+1, MinY, MaxX, MaxY, Acc1);
        Y =:= MinY andalso DY =:= -1 ->
            spiral(X+1, Y, 1, 0, MinX, MinY+1, MaxX, MaxY, Acc1);
        Y > MaxY+1 orelse X > MaxX+1 orelse X < -2 orelse Y < -2 ->
            ok;
        true ->
            spiral(X+DX, Y+DY, DX, DY, MinX, MinY, MaxX, MaxY, Acc1)
    end.

fade_in_title(Title) ->
    {CX,CY} = ru_util:centering_coords(length(Title), 1),
    MapChars = fun(Char, Acc) ->
        [{length(Acc), Char} | Acc]
    end,
    Mapped = lists:foldl(MapChars, [], Title),
    Draw = fun({X, Char}) ->
        encurses:move(CX+X, CY-2),
        encurses:vline($\s, 5),
        encurses:mvaddch(CX+X, CY, Char),
        encurses:refresh(),
        timer:sleep(10)
    end,
    random:seed(now()),
    Randomize = fun(_,_) ->
        random:uniform(2) =:= 1
    end,
    lists:foreach(Draw, lists:sort(Randomize, Mapped)).

do_splash_screen() ->
    encurses:erase(),
    encurses:curs_set(?CURS_INVISIBLE),
    {MX,MY} = ru_util:get_window_dimensions(),
    spiral(0, 0, 1, 0, 0, 0, MX-1, MY-1, 0),
    encurses:refresh(),
    fade_in_title(" R O G U E U N L I K E "),
    Me = self(),
    ru_input:set_mode(fun(_,_) -> Me ! ok end),
    receive
        ok -> ok
    after 
        2000 -> ok
    end,
    encurses:erase(),
    encurses:refresh(),
    ok.

