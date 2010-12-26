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

-module(ru).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include_lib("deps/encurses/include/encurses.hrl").
-include("ru.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([start_link/0]).
-export([start/0, stop/1, redraw/1, tick/0, go/0]).

-record(state, {turn=0}).

-define(DIENAME, never_die).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(_Reason) ->
    ru_draw:cleanup(),
    ?DIENAME ! die,
    application:stop(rogueunlike).

tick() ->
    gen_server:call(?MODULE, tick).

redraw(Reason) ->
    gen_server:cast(?MODULE, {redraw, Reason}).

start() ->
    gen_server:call(?MODULE, start),
    register(?DIENAME, self()),
    wait_to_die().

go() ->
    %encurses:initscr(),
    %ncurses:keypad(0, true),
    %encurses:noecho(),
    %splash_screen(),
    application:start(rogueunlike),
    ru_draw:init().

%% ============================================================================
%% gen_server Behavior
%% ============================================================================

init(_) ->
    {ok, #state{}}.

handle_call(start, _From, State) ->
    {reply, do_start_stuff(), State};
handle_call(tick, _From, State) ->
    {reply, ok, do_tick(State)}.

handle_cast({redraw, Reason}, State) ->
    do_redraw(Reason),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    never_die ! die,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

wait_to_die() ->
    receive die -> ok end.

do_start_stuff() ->
    make_hero(),
    ConsoleHeight = 6,
    ru_console:create(ConsoleHeight),
    ?MSG("Press q to quit!"),
    ru_input:set_mode({ru_input, game_mode}),
    ru_world:init_world(ConsoleHeight),
    ru_world:database_test(),
    ru_state:add_hero({1,1}),
    make_dog(),
    make_zombie(),
    do_redraw(init),
    ok.

do_tick(State) ->
    ?MSG(io_lib:format("Turn ~p", [State#state.turn+1])),
    ru_mobs:tick(),
    ru_world:tick(),
    State#state{ turn=State#state.turn + 1}.

do_redraw(Reason) ->
    ru_draw:draw(Reason),
    ru_world:redraw(Reason),
    ru_console:redraw(Reason),
    ok.

make_hero() ->
    Char = #cstats{ 
        name = "Gravlax", 
        gender = male, 
        race = "Troll",
        level = 1, 
        gold = 100, 
        hp = 20, 
        hpmax = 20,
        attributes = #cattributes { 
            strength = 10,
            dexterity = 7,
            constitution = 5,
            intelligence = 4,
            wisdom = 2,
            charisma = 2 }},
    ru_char:set_char(Char).

make_dog() ->
    ru_state:add_mob(dog, {2,1}, fun ru_brains:dog_brain/2).

make_zombie() ->
    ru_state:add_mob(zombie, {19,5}, fun ru_brains:zombie_brain/2).

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

splash_screen() ->
    encurses:erase(),
    encurses:curs_set(?CURS_INVISIBLE),
    {MX,MY} = ru_util:get_window_dimensions(),
    spiral(0, 0, 1, 0, 0, 0, MX-1, MY-1, 0),
    encurses:refresh(),
    fade_in_title(" R O G U E U N L I K E "),
    encurses:getch(),
    encurses:erase(),
    encurses:refresh(),
    ok.

