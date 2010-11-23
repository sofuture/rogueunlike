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

-include("cecho.hrl").
-include("ru.hrl").

-export([go/0, die/0]).
-export([resize_loop/0, exit/1, redraw/1, tick/0]).

-record(state, {turn=0}).

%% ============================================================================
%% Module API
%% ============================================================================

go() ->
    init(),
    splash_screen(),
    start_systems(),
    make_hero(),
    ConsoleHeight = 6,
    ru_console:create(ConsoleHeight),
    ?MSG("Press q to quit!"),
    ru_input:set_mode({ru_input, game_mode}),
    ru_world:init(ConsoleHeight),
    ru_world:database_test(),
    ru_state:add_hero({1,1}),
    make_dog(),
    make_zombie(),
    ru_world:redraw(init),
    main_loop(#state{}).

die() ->
    ru_console:exit(die),
    ru_char:exit(die),
    ru_input:exit(die),
    encurses:erase(),
    encurses:refresh(),
    encurses:endwin(),
    halt().

%% ============================================================================
%% Internal Functions
%% ============================================================================

make_hero() ->
    Char = #cstats{ name="Gravlax", gender=male, race="Troll",
        level=1, gold=100, hp=20, hpmax=20},
    ru_char:set_char(Char).

make_dog() ->
    ru_state:add_mob(dog, {2,1}, fun ru_brains:dog_brain/2).

make_zombie() ->
    ru_state:add_mob(zombie, {19,5}, fun ru_brains:zombie_brain/2).

main_loop(State) ->
    receive
        {tick, _} ->
            ?MSG(io_lib:format("Turn ~p", [State#state.turn+1])),
            ru_mobs:tick(),
            ru_world:tick(),
            main_loop(State#state{ turn=State#state.turn + 1});

        {redraw, Reason} ->
            ru_input:redraw(Reason),
            ru_world:redraw(Reason),
            ru_console:redraw(Reason),
            encurses:refresh(),
            main_loop(State);

        {exit, _} ->
            die();

        _ -> 
            main_loop(State)
    end.

tick() ->
    ?MODULE ! {tick, tock}.

redraw(Reason) ->
    ?MODULE ! {redraw, Reason}.

exit(Reason) ->
    ?MODULE ! {exit, Reason}.

% listen for SIGWINCH at window resize

resize_loop() ->
    encurses:sigwinch(),
    redraw(sigwinch),
    resize_loop().

init() ->
    encurses:initscr(),
    encurses:keypad(0, true),
    encurses:noecho(),
    ok.

start_systems() ->
    ru_console:start(),
    ru_char:start(),
    ru_input:start(),
    ru_world:start(),
    ru_state:start(),
    ru_mobs:start(),
    start_self().

start_self() ->
    %spawn(?MODULE, resize_loop, []),
    true = register(?MODULE, self()),
    ok.

spiral(X,Y, DX, DY, MinX, MinY, MaxX, MaxY, Acc) ->
    encurses:mvaddch(X,Y,$=),
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
    encurses:curs_set(?ceCURS_INVISIBLE),
    {MX,MY} = ru_util:get_window_dimensions(),
    spiral(0, 0, 1, 0, 0, 0, MX-1, MY-1, 0),
    encurses:refresh(),
    fade_in_title(" R O G U E U N L I K E "),
    encurses:getch(),
    encurses:erase(),
    encurses:refresh(),
    ok.

