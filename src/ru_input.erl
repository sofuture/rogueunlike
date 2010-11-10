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

-module(ru_input).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("ru.hrl").

-export([start/0,exit/1]).
-export([set_mode/1, key_loop/1, redraw/1, input/1]).
-export([script_mode/2, game_mode/2]).
-export([recv_loop/2]).

-record(input, {
        flags = [],
        buffer = []}).

%% ============================================================================
%% Module API
%% ============================================================================

set_mode(Mode) ->
    ?MODULE ! {mode, Mode}.

redraw(Reason) ->
    ?MODULE ! {redraw, Reason}.

input(Input) ->
    ?MODULE ! {input, Input}.

exit(Reason) ->
    ?MODULE ! {exit, Reason}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

start() ->
    true = register(keyreader, 
        spawn(?MODULE, key_loop, [[]])),
    true = register(?MODULE,
        spawn(?MODULE, recv_loop, [fun(_) -> ok end, #input{}])).

recv_loop(Mode, State) ->
    receive
        {exit, _} -> 
            ok;

        {mode, NewMode} ->
            %% TODO maybe default clear state here?
            recv_loop(NewMode, State);

        {input, Input} ->
            Mode(Input, State),
            recv_loop(Mode, State);

        {redraw, _Reason} ->
            recv_loop(Mode, State);
            
        _ -> 
            recv_loop(Mode, State)
    end.

key_loop(Buffer) ->
    cecho:noecho(),
    input(cecho:getch()),
    key_loop(Buffer).

parse_direction(Char) ->
    case Char of
        262 -> kp_nw;
        $y -> kp_nw;
        $7 -> kp_nw;

        259 -> kp_n;
        $k -> kp_n;
        $8 -> kp_n;
        
        339 -> kp_ne;
        $u -> kp_ne;
        $9 -> kp_ne;
        
        260 -> kp_w;
        $h -> kp_w;
        $4 -> kp_w;
        
        350 -> kp_center;
        $. -> kp_center;
        $5 -> kp_center;
        
        261 -> kp_e;
        $l -> kp_e;
        $6 -> kp_e;
        
        360 -> kp_sw;
        $b -> kp_sw;
        $1 -> kp_sw;
        
        258 -> kp_s;
        $j -> kp_s;
        $2 -> kp_s;
        
        338 -> kp_se;
        $n -> kp_se;
        $3 -> kp_se;
        
        Other -> Other
    end.

%% ============================================================================
%% Input Modes
%% ============================================================================

script_mode(Input, _State) ->
    case Input of
        $q -> ru:exit("Got exit message");
        _ -> script ! {dosomething, nil}
    end.

attack_cmd_mode(Input, _State) ->
    DirInput = parse_direction(Input),
    case DirInput of
        Dir when ?ISDIR(Dir) ->
            case ru_state:attack(hero, Dir) of 
                ok -> ?MSG("Bam! Bang! Crash!");
                nomob -> ?MSG("Woosh.");
                nil -> ok
            end,
            ru:tick();
        _ -> ok
    end,
    set_mode(fun game_mode/2).

close_cmd_mode(Input, _State) ->
    DirInput = parse_direction(Input),
    case DirInput of
        Dir when ?ISDIR(Dir) ->
            case ru_state:close_door(Dir) of 
                ok -> ?MSG("The door slams shut.");
                nodoor -> ?MSG("Ain't nothin' to close there!");
                error -> ?MSG("Try as you might, it won't close.")
            end,
            ru:tick();
        _ -> ok
    end,
    set_mode(fun game_mode/2).

open_cmd_mode(Input, _State) ->
    DirInput = parse_direction(Input),
    case DirInput of
        Dir when ?ISDIR(Dir) ->
            case ru_state:open_door(Dir) of
                ok -> ?MSG("The door creaks open.");
                nodoor -> ?MSG("Yeah... I can't open that.");
                error -> ?MSG("It won't budge.")
            end,
            ru:tick();
        _ -> ok
    end,
    set_mode(fun game_mode/2).

game_mode(Input, _State) ->
    DirInput = parse_direction(Input),
    case DirInput of
        $Q -> 
            ru:exit("Got exit message");

        Dir when ?ISDIR(Dir) ->
            case Dir of
                kp_center -> ru:tick();
                _ -> 
                    ru_state:move(hero, Dir),
                    ru:tick()
            end;

        Action when Action =:= $? ->
            Win = ru_menu:draw(ru_text:help_menu()),
            CloseClosure = fun(_,_) ->
                ru_menu:undraw(Win),
                ru:redraw(menu),
                set_mode(fun game_mode/2)
            end,
            ?MSG("Opening help... press every key to close"),
            set_mode(CloseClosure);

        Action when Action =:= $A ->
            Win = ru_menu:draw(ru_text:about_menu()),
            CloseClosure = fun(_,_) ->
                ru_menu:undraw(Win),
                ru:redraw(menu),
                set_mode(fun game_mode/2)
            end,
            ?MSG("Opening about... press a key labeled 'Any' to close"),
            set_mode(CloseClosure);

        DirectedAction when 
                DirectedAction =:= $o orelse
                DirectedAction =:= $c orelse
                DirectedAction =:= $a ->
            ?MSG("In which direction?"),
            case DirectedAction of
                $o -> set_mode(fun open_cmd_mode/2);
                $c -> set_mode(fun close_cmd_mode/2);
                $a -> set_mode(fun attack_cmd_mode/2)
            end;

        _ -> ok
    end.

