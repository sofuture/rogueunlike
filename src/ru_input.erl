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
        $7 -> kp_nw;
        $8 -> kp_n;
        $9 -> kp_ne;
        $4 -> kp_w;
        $5 -> kp_center;
        $6 -> kp_e;
        $1 -> kp_sw;
        $2 -> kp_s;
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

game_mode(Input, _State) ->
    case Input of
        $Q -> ru:exit("Got exit message");
        Dir when Dir =:= kp_n orelse Dir =:= kp_s orelse Dir =:= kp_e orelse
            Dir =:= kp_w orelse Dir =:= kp_nw orelse Dir =:= kp_ne orelse
            Dir =:= kp_sw orelse Dir =:= kp_se ->
                ru_state:move_hero(Dir);

        _ -> ru_console:msg(?PP(Input))
    end.

