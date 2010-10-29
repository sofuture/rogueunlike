%% ============================================================================
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
    Char = cecho:getch(),
    RetChar = case Char of
        262 -> kp_nw;
        $7 -> kp_nw;

        259 -> kp_n;
        $8 -> kp_n;
        
        339 -> kp_ne;
        $9 -> kp_ne;
        
        260 -> kp_w;
        $4 -> kp_w;
        
        350 -> kp_center;
        $5 -> kp_center;
        
        261 -> kp_e;
        $6 -> kp_e;
        
        360 -> kp_sw;
        $1 -> kp_sw;
        
        258 -> kp_s;
        $2 -> kp_s;
        
        338 -> kp_se;
        $3 -> kp_se;
        
        Other -> Other
    end,
    input(RetChar),
    key_loop(Buffer).

%% ============================================================================
%% Input Modes
%% ============================================================================

script_mode(Input, _State) ->
    case Input of
        $q -> ru:exit("Got exit message");
        _ -> script ! {dosomething, nil}
    end.

open_cmd_mode(Input, _State) ->
    case Input of
        Dir when Dir =:= kp_n orelse Dir =:= kp_s orelse Dir =:= kp_e orelse
            Dir =:= kp_w orelse Dir =:= kp_nw orelse Dir =:= kp_ne orelse
            Dir =:= kp_sw orelse Dir =:= kp_se ->
                case ru_state:open_door(Dir) of
                    ok -> ru_console:msg("The door creaks open.");
                    nodoor -> ru_console:msg("Yeah... I can't open that.");
                    error -> ru_console:msg("It won't budge.")
                end;
        _ -> ok
    end,
    set_mode({ru_input, game_mode}).

game_mode(Input, _State) ->
    case Input of
        $Q -> 
            ru:exit("Got exit message");

        Dir when Dir =:= kp_n orelse Dir =:= kp_s orelse Dir =:= kp_e orelse
            Dir =:= kp_w orelse Dir =:= kp_nw orelse Dir =:= kp_ne orelse
            Dir =:= kp_sw orelse Dir =:= kp_se ->
                ru_state:move_hero(Dir);
        
        Action when Action =:= $o orelse Action =:= $O ->
            ru_console:msg("In which direction?"),
            set_mode(fun open_cmd_mode/2); 

        _ -> ru_console:msg(?PP(Input))
    end.

